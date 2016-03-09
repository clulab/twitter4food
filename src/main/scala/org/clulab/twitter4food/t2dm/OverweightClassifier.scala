package org.clulab.twitter4food.t2dm

import java.io.PrintWriter

import edu.arizona.sista.learning.{LinearSVMClassifier, RVFDataset}
import edu.arizona.sista.struct.Counter
import org.clulab.twitter4food.featureclassifier.FeatureClassifier
import org.clulab.twitter4food.struct.{Tweet, FeatureExtractor, TwitterAccount}

/**
  * Created by Terron on 2/15/16.
  */
class OverweightClassifier(
                              val useUnigrams: Boolean = true,
                              val useBigrams: Boolean = false,
                              val useTopics: Boolean = false,
                              val useDictionaries: Boolean = false,
                              val useEmbeddings: Boolean = false) extends FeatureClassifier {

    val featureExtractor = new FeatureExtractor(useUnigrams, useBigrams, useTopics, useDictionaries, useEmbeddings)
    val subClassifier = new LinearSVMClassifier[String, String]()
    var dataset = new RVFDataset[String, String]()

    override def train(accounts: Seq[TwitterAccount], labels: Seq[String]): Unit = {
        assert(accounts.size == labels.size)

        // Clear current dataset if training on new one
        dataset = new RVFDataset[String, String]()
        // Populate dataset
        for (i <- accounts.indices) {
            dataset += featureExtractor.mkDatum(accounts(i), labels(i))
        }
        subClassifier.train(dataset)
    }

    override def scoresOf(account: TwitterAccount): Counter[String] = {
        subClassifier.scoresOf(featureExtractor.mkDatum(account, "unknown"))
    }
}

object OverweightClassifier {

    def main(args: Array[String]) {
        var unigrams = false
        var bigrams = false

        var fileLabel = ""

        for (arg <- args) {
            fileLabel += arg
            if (arg equals "1")
                unigrams = true
            if (arg equals "2")
                bigrams = true
        }

        println("useUnigrams=" + unigrams)
        println("useBigrams=" + bigrams)

        val oc = new OverweightClassifier(useUnigrams = unigrams, useBigrams = bigrams)

        // when running on local machine
//        val trainFile = "src/main/resources/org/clulab/twitter4food/featureclassifier/overweight/overweightTrain.txt"
//        val devFile = "src/main/resources/org/clulab/twitter4food/featureclassifier/overweight/overweightDev.txt"
//        val testFile = "src/main/resources/org/clulab/twitter4food/featureclassifier/overweight/overweightTest.txt"

        // when running on servers
        val trainFile = "/data/nlp/corpora/twitter4food/overweightData/overweightTrain.txt"
        val devFile = "/data/nlp/corpora/twitter4food/overweightData/overweightDev.txt"
        val testFile = "/data/nlp/corpora/twitter4food/overweightData/overweightTest.txt"

        println("Reading in training accounts...")
        val trainAccounts = OverweightDataExtraction.parse(trainFile)
        println("Reading in dev accounts...")
        val devAccounts = OverweightDataExtraction.parse(devFile)
        //        println("Reading in test accounts...")
        //        val testAccounts = OverweightDataExtraction.parse(testFile)

        println("Training classifier...")
        oc.train(trainAccounts.keys.toSeq, trainAccounts.values.toSeq)

        println("Running classifications on dev accounts...")

        // Only checking for correct overweight labels
        var numRelevant = 0
        var truePositives = 0
        var falsePositives = 0

        for ((account, label) <- devAccounts) {
            if (oc.classify(account) equals "Overweight") {
                if (label equals "Overweight") {
                    truePositives += 1
                } else {
                    falsePositives += 1
                }
            }

            if (label equals "Overweight") {
                numRelevant += 1
            }
        }

        val precision = truePositives * 1.0 / (truePositives + falsePositives)
        val recall = truePositives * 1.0 / (numRelevant)

        println("\nResults:")
        println(s"Precision: ${precision}")
        println(s"Recall: ${recall}")
        println(s"F-measure (harmonic mean): ${fMeasure(precision, recall, 1)}")
        println(s"F-measure (recall 5x): ${fMeasure(precision, recall, .2)}")

        val writer = new PrintWriter("output" + fileLabel + ".txt")
        writer.write(s"Precision: ${precision}\n")
        writer.write(s"Recall: ${recall}\n")
        writer.write(s"F-measure (harmonic mean): ${fMeasure(precision, recall, 1)}\n")
        writer.write(s"F-measure (recall 5x): ${fMeasure(precision, recall, .2)}\n")

        writer.close()

    }

    private def fMeasure(precision: Double, recall: Double, beta: Double): Double =
        (1 + Math.pow(beta, 2)) * ((precision * recall) / (Math.pow(beta, 2) * precision + recall))

}