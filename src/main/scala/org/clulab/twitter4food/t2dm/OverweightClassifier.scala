package org.clulab.twitter4food.t2dm

import java.io.{BufferedWriter, FileWriter, PrintWriter}
import java.nio.file.{Files, Paths}

import edu.arizona.sista.learning.{LiblinearClassifier, LinearSVMClassifier, RVFDataset}
import edu.arizona.sista.struct.Counter
import org.clulab.twitter4food.featureclassifier.FeatureClassifier
import org.clulab.twitter4food.struct.{FeatureExtractor, Tweet, TwitterAccount}
import org.clulab.twitter4food.util.{Eval, FileUtils, TestUtils}

/**
  * Created by Terron on 2/15/16.
  */
class OverweightClassifier(val useUnigrams: Boolean = true,
                           val useBigrams: Boolean = false,
                           val useTopics: Boolean = false,
                           val useDictionaries: Boolean = false,
                           val useEmbeddings: Boolean = false) extends FeatureClassifier {

    val featureExtractor = new FeatureExtractor(useUnigrams, useBigrams, useTopics, useDictionaries, useEmbeddings)
    var subClassifier = new LinearSVMClassifier[String, String]()
    var dataset = new RVFDataset[String, String]()

    override def train(accounts: Seq[TwitterAccount], labels: Seq[String]): Unit = {
        assert(accounts.size == labels.size)

        // Clear current dataset if training on new one
        dataset = new RVFDataset[String, String]()

        val pb = new me.tongfei.progressbar.ProgressBar("train()", 100)
        pb.start()
        pb.maxHint(accounts.size)
        pb.setExtraMessage("Training...")

        // Populate dataset
        for (i <- accounts.indices) {
            dataset += featureExtractor.mkDatum(accounts(i), labels(i))
            pb.step()
        }

        pb.stop()
        subClassifier.train(dataset)
    }

    override def scoresOf(account: TwitterAccount): Counter[String] = {
        subClassifier.scoresOf(featureExtractor.mkDatum(account, "unknown"))
    }
}

object OverweightClassifier {

    def main(args: Array[String]) {
        val params = TestUtils.parseArgs(args)
        val (api, config) = TestUtils.init(0, true)
        val oc = new OverweightClassifier(params.useUnigrams, params.useBigrams,
            params.useTopics, params.useDictionaries, params.useEmbeddings)

        val fileExt = args.mkString("")
        val modelFile = s"${config.getString("classifier")}/overweight/model/${fileExt}.dat"

        // Load classifier if model exists
        if (Files.exists(Paths.get(modelFile))) {
            println("Loading model from file...")
            val cl = LiblinearClassifier.loadFrom[String, String](modelFile)
            oc.subClassifier = new LinearSVMClassifier[String, String](C=cl.C, eps=cl.eps, bias=cl.bias)
        } else {
            println("Loading training accounts...")
            val trainingData = FileUtils.load(config.getString("classifiers.overweight.trainingData"))

            // Train classifier and save model to file
            println("Training classifier...")
            oc.train(trainingData.keys.toSeq, trainingData.values.toSeq)
            oc.subClassifier.saveTo(modelFile)
        }

        println(s"${oc.subClassifier.getWeights()}")

        println("Loading dev accounts...")
        val devData = FileUtils.load(config.getString("classifiers.overweight.devData"))
//        println("Loading test accounts...")
//        val testAccounts = FileUtils.load(config.getString("classifiers.overweight.testData"))

        // Set progress bar
        val pb = new me.tongfei.progressbar.ProgressBar("main()", 100)
        pb.start()
        pb.maxHint(devData.size)
        pb.setExtraMessage("Testing on dev accounts...")

        // Classify accounts
        val devLabels = devData.values.toSeq
        val predictedLabels = devData.keys.toSeq.map(u => { pb.step(); oc.classify(u); })

        pb.stop()

        println("Dev:")
        println(devLabels.mkString("\t"))
        println("\nPredicted:")
        println(predictedLabels.mkString("\t"))

        val (evalMeasures, microAvg, macroAvg) = Eval.evaluate(devLabels, predictedLabels,
            devData.keys.toSeq)

        println(evalMeasures.mkString("\n"))

        val evalMetric = evalMeasures("Overweight")

        val precision = evalMetric.P
        val recall = evalMetric.R

        println("\nResults:")
        println(s"Precision: ${precision}")
        println(s"Recall: ${recall}")
        println(s"F-measure (harmonic mean): ${fMeasure(precision, recall, 1)}")
        println(s"F-measure (recall 5x): ${fMeasure(precision, recall, .2)}")
        println(s"Macro average: ${macroAvg}")
        println(s"Micro average: ${microAvg}")

        // Save results
        val writer = new BufferedWriter(new FileWriter(
            config.getString("classifier") + "/overweight/results/output" +
                fileExt + ".txt",true))
        writer.write(s"Precision: ${precision}\n")
        writer.write(s"Recall: ${recall}\n")
        writer.write(s"F-measure (harmonic mean): ${fMeasure(precision, recall, 1)}\n")
        writer.write(s"F-measure (recall 5x): ${fMeasure(precision, recall, .2)}\n")
        writer.write(s"Macro average: ${macroAvg}")
        writer.write(s"Micro average: ${microAvg}")
        writer.close()

    }

    private def fMeasure(precision: Double, recall: Double, beta: Double): Double =
        (1 + Math.pow(beta, 2)) * ((precision * recall) / (Math.pow(beta, 2) * precision + recall))

}