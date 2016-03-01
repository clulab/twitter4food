package org.clulab.twitter4food.t2dm

import edu.arizona.sista.learning.{LinearSVMClassifier, RVFDataset}
import edu.arizona.sista.struct.Counter
import org.clulab.twitter4food.featureclassifier.FeatureClassifier
import org.clulab.twitter4food.struct.{Tweet, FeatureExtractor, TwitterAccount}

/**
  * Created by Terron on 2/15/16.
  */
class OverweightClassifier(
    val useUnigrams:Boolean = true,
    val useBigrams:Boolean = false,
    val useTopics:Boolean = false,
    val useDictionaries:Boolean = false,
    val useEmbeddings:Boolean = false) extends FeatureClassifier {

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
        val oc = new OverweightClassifier()

        println("Reading in training accounts...")
        val trainAccounts = OverweightDataExtraction.parse("src/main/resources/org/clulab/twitter4food/featureclassifier/overweight/overweightTrain.txt")
        println("Reading in dev accounts...")
        val devAccounts = OverweightDataExtraction.parse("src/main/resources/org/clulab/twitter4food/featureclassifier/overweight/overweightDev.txt")
//        println("Reading in test accounts...")
//        val testAccounts = OverweightDataExtraction.parse("src/main/resources/org/clulab/twitter4food/featureclassifier/overweight/overweightTest.txt")

        println("Training classifier...")
        oc.train(trainAccounts.keys.toSeq, trainAccounts.values.toSeq)

        println("Running classifications...")

        var numCorrect = 0
        var total = devAccounts.size

        for ((account, label) <- devAccounts) {
            if (oc.classify(account) equals label) numCorrect += 1
        }

        println(s"\nResults: ${numCorrect * 1.0 / total} percent correctly classified")

    }
}