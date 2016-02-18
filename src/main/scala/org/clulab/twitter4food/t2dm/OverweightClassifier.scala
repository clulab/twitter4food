package org.clulab.twitter4food.t2dm

import edu.arizona.sista.learning.{LinearSVMClassifier, RVFDataset}
import edu.arizona.sista.struct.Counter
import org.clulab.twitter4food.featureclassifier.FeatureClassifier
import org.clulab.twitter4food.struct.{FeatureExtractor, TwitterAccount}
import org.clulab.twitter4food.twitter4j.TwitterAPI

import scala.io.Source

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

    val api = new TwitterAPI(0)

    def main(args: Array[String]) {
        val oc = new OverweightClassifier()

        var numOverweight = 0
        var numNotOverweight = 0

        for (line <- Source.fromFile("src/main/resources/org/clulab/twitter4food/featureclassifier/overweight/overweightData.txt").getLines){
            val tuple = line.split("\t")
            val classification = tuple(1)
            if (classification equals "Overweight")
                numOverweight += 1
            else if (classification equals "Not overweight")
                numNotOverweight += 1
        }

        // Get list of handles to analyze
        var trainAccounts: List[TwitterAccount] = List()
        var trainLabels: List[String] = List()
        val trainPercent = 0.80

        val percentOverweight = 0.50

        var testAccounts: List[TwitterAccount] = List()
        var testLabels: List[String] = List()

        println("Reading in handles from data file...")

        var classification = ""

        var handle = ""
        var id = 0L
        var name = ""
        var lang = ""
        var location = ""
        var description = ""

        var first = true

        for (line <- Source.fromFile("src/main/resources/org/clulab/twitter4food/featureclassifier/overweight/overweightDataVerbose.txt").getLines){
            if ( (line equals "Overweight") || (line equals "Not overweight") ) {
                description = ""
                classification = line
                if (!first) {
//                    if () {
//
//                    }
                }
            }
            else if (line.split("\t").length > 3) {
                var attr = line.split("\t")
                var handle = attr(0)
                var id = attr(1)
                var name = attr(2)
                var lang = attr(3)
                var location = attr(4)
            }
            else {
                description += line
            }
        }

        println("Training accounts...")
        oc.train(trainAccounts, trainLabels)

        println("Results:")
        println("Predicted\tActual")
        for (i <- testAccounts.indices) {
            println(s"${oc.classify(testAccounts(i))}\t${testLabels(i)}")
        }

    }
}