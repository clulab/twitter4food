package org.clulab.twitter4food.t2dm

import java.io.{BufferedWriter, File, FileWriter, PrintWriter}
import java.nio.file.{Files, Paths}

import com.typesafe.config.ConfigFactory
import edu.arizona.sista.learning.{LiblinearClassifier, LinearSVMClassifier, RVFDataset}
import edu.arizona.sista.struct.Counter
import org.clulab.twitter4food.featureclassifier.FeatureClassifier
import org.clulab.twitter4food.struct.{FeatureExtractor, Tweet, TwitterAccount}
import org.clulab.twitter4food.util.{Eval, EvalMetric, FileUtils, TestUtils}

/**
  * Created by Terron on 2/15/16.
  */
class OverweightClassifier(val useUnigrams: Boolean = true,
                           val useBigrams: Boolean = false,
                           val useTopics: Boolean = false,
                           val useDictionaries: Boolean = false,
                           val useEmbeddings: Boolean = false,
                           val useCosineSim: Boolean) extends FeatureClassifier {

    val featureExtractor = new FeatureExtractor(useUnigrams, useBigrams, useTopics, useDictionaries, useEmbeddings, useCosineSim)
    var subClassifier : Option[LiblinearClassifier[String, String]] = None
    var dataset = new RVFDataset[String, String]()

    override def train(accounts: Seq[TwitterAccount], labels: Seq[String]): Unit = {
        assert(accounts.size == labels.size)

        // Clear current dataset if training on new one
        dataset = new RVFDataset[String, String]()
        subClassifier = Some(new LinearSVMClassifier[String, String]())

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
        subClassifier.get.train(dataset)
    }

    override def scoresOf(account: TwitterAccount): Counter[String] = {
        subClassifier.get.scoresOf(featureExtractor.mkDatum(account, "unknown"))
    }

    def analyze(modelFile: String, labels: Set[String], test: TwitterAccount, numTopWeights: Int, numTopScores: Int) = {
        val c = LiblinearClassifier.loadFrom[String, String](modelFile)
        val W = c.getWeights()
        val d = featureExtractor.mkDatum(test, "unknown")
        val counter = d.featuresCounter

        var topWeights = labels.foldLeft(Map[String, Seq[(String, Double)]]())(
            (map, l) => map + (l -> W.get(l).get.toSeq.sortWith(_._2 > _._2)))
        topWeights = topWeights.slice(0, numTopWeights)

        var dotProduct = labels.foldLeft(Map[String, Seq[(String, Double)]]())(
            (map, l) => {
                val weightMap = W.get(l).get.toSeq.toMap
                val feats = d.featuresCounter.toSeq
                map + (l -> feats.filter(f => weightMap.contains(f._1))
                    .map(f => (f._1, f._2 * weightMap(f._1))).sortWith(_._2 > _._2))
            })
        dotProduct = dotProduct.slice(0, numTopScores)

        (topWeights, dotProduct)
    }
}

object OverweightClassifier {

    def main(args: Array[String]) {
        // Parse args using standard Config
        val params = TestUtils.parseArgs(args)
        val (api, config) = TestUtils.init(0, true)
        val oc = new OverweightClassifier(params.useUnigrams, params.useBigrams,
            params.useTopics, params.useDictionaries, params.useEmbeddings, params.useCosineSim)

        val fileExt = args.sorted.mkString("")
        val modelFile = s"${config.getString("classifier")}/overweight/model/m${fileExt}.dat"

        // Allow user to specify if model should be loaded or overwritten
        var loadModel = false
        print("\n\nLoad model from existing file? (yes/no) ")
        val answer = scala.io.StdIn.readLine()
        if (answer.toLowerCase.charAt(0) == 'y')
            loadModel = true

        // Load classifier if model exists
        if ( loadModel && Files.exists(Paths.get(modelFile)) ) {
            println("Loading model from file...")
            val cl = LiblinearClassifier.loadFrom[String, String](modelFile)
            oc.subClassifier = Some(cl)
        } else {
            println("Loading training accounts...")
            val trainingData = FileUtils.load(config.getString("classifiers.overweight.trainingData"))

            // Train classifier and save model to file
            println("Training classifier...")
            oc.train(trainingData.keys.toSeq, trainingData.values.toSeq)
            oc.subClassifier.get.saveTo(modelFile)
        }

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

        // Print results
        val (evalMeasures, microAvg, macroAvg) = Eval.evaluate(devLabels, predictedLabels,
            devData.keys.toSeq)

        val evalMetric = evalMeasures("Overweight")
        val precision = evalMetric.P
        val recall = evalMetric.R

        val outputDir = config.getString("classifier") + "/overweight/results/r" + fileExt
        if (!Files.exists(Paths.get(outputDir))) {
            if (new File(outputDir).mkdir())
                println(s"Created output directory ${outputDir}")
            else
                println(s"ERROR: failed to create output directory ${outputDir}")
        }

        println("False negatives:")
        evalMetric.FNAccounts.map(account => print(account.handle + "\t"))
        println("\n====")
        outputAnalysis(outputDir + "/analysisFN.txt", modelFile,
            "*** False negatives ***\n\n", evalMetric.FNAccounts, oc)

        println("False positives:")
        evalMetric.FPAccounts.map(account => print(account.handle + "\t"))
        println("\n====")
        outputAnalysis(outputDir + "/analysisFP.txt", modelFile,
            "*** False positives ***\n\n", evalMetric.FPAccounts, oc)

        println("\nResults:")
        println(s"Precision: ${precision}")
        println(s"Recall: ${recall}")
        println(s"F-measure (harmonic mean): ${fMeasure(precision, recall, 1)}")
        println(s"F-measure (recall 5x): ${fMeasure(precision, recall, .2)}")
        println(s"Macro average: ${macroAvg}")
        println(s"Micro average: ${microAvg}")

        // Save results
        val writer = new BufferedWriter(new FileWriter(
            config.getString("classifier") + "/overweight/results/r"+fileExt+"/analysisMetrics.txt", false))
        writer.write(s"Precision: ${precision}\n")
        writer.write(s"Recall: ${recall}\n")
        writer.write(s"F-measure (harmonic mean): ${fMeasure(precision, recall, 1)}\n")
        writer.write(s"F-measure (recall 5x): ${fMeasure(precision, recall, .2)}\n")
        writer.write(s"Macro average: ${macroAvg}\n")
        writer.write(s"Micro average: ${microAvg}\n")
        writer.close()

    }

    private def outputAnalysis(outputFile:String, modelFile: String, header:String, accounts: Seq[TwitterAccount], oc: OverweightClassifier): Unit = {
        // Set progress bar
        var numAccountsToPrint = 20
        val numWeightsToPrint = 30
        val pb = new me.tongfei.progressbar.ProgressBar("outputAnalysis()", 100)
        pb.start()
        pb.maxHint(numAccountsToPrint)
        pb.setExtraMessage(header)

        // Initialize writer
        val writer = new BufferedWriter(new FileWriter(outputFile, false))
        var isFirst = true
        writer.write(header)

        // Iterate over accounts
        for (account <- accounts) {
            if (numAccountsToPrint > 0) {
                // Analyze account
                val (topWeights, dotProduct) = oc.analyze(modelFile, Set[String]("Overweight", "Not overweight"),
                    account, numWeightsToPrint, numWeightsToPrint)
                // Only print the general weights on the features once
                if (isFirst) {
                    for ((label, sequence) <- topWeights) {
                        writer.write(s"Top weights for ${label}:\n")
                        var numToPrint = numWeightsToPrint
                        for ((feature, score) <- sequence) {
                            if ((numToPrint > 0) && (score > 0.0)) {
                                writer.write(s"${feature} -> ${score}\n")
                                numToPrint = numToPrint - 1
                            }
                        }
                        writer.write("================================\n")
                    }
                    isFirst = false
                }
                // Print hadamard product for every account
                writer.write(s"Hadamard product for ${account.handle}:\n")
                for ((label, sequence) <- dotProduct) {
                    if (label equals "Overweight") {
                        var numToPrint = numWeightsToPrint
                        for ((feature, score) <- sequence) {
                            if ((numToPrint > 0) && (score > 0.0)) {
                                writer.write(s"${feature} -> ${score}\n")
                                numToPrint = numToPrint - 1
                            }
                        }
                    }
                }
                writer.write("================================\n")
            }
            pb.step()
            numAccountsToPrint -= 1
        }
        writer.close
        pb.stop()
    }

    private def fMeasure(precision: Double, recall: Double, beta: Double): Double =
        (1 + Math.pow(beta, 2)) * ((precision * recall) / (Math.pow(beta, 2) * precision + recall))

}