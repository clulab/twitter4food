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
class OverweightClassifier extends FeatureClassifier {

    val useUnigrams = true
    val useBigrams = false
    val useTopics = false
    val useDictionaries = false
    val useEmbeddings = false

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

        // Calculate total number of overweight / not overweight accounts
        var totalOverweight = 0
        var totalNotOverweight = 0

        for (line <- Source.fromFile("src/main/resources/org/clulab/twitter4food/featureclassifier/overweight/overweightData.txt").getLines){
            val tuple = line.split("\t")
            val classification = tuple(1)
            if (classification equals "Overweight")
                totalOverweight += 1
            else if (classification equals "Not overweight")
                totalNotOverweight += 1
        }

        // Get training and testing sets
        var trainAccounts: List[TwitterAccount] = List()
        var trainLabels: List[String] = List()
        val trainPercent = 0.90
//        var numTrainOverweight = 0
//        var numTrainNotOverweight = 0
//
//        val percentOverweight = 0.50

        var testAccounts: List[TwitterAccount] = List()
        var testLabels: List[String] = List()
//        var numTestOverweight = 0
//        var numTestNotOverweight = 0

        println("Reading in handles from data file...")

        var classification = ""

        var handle = ""
        var id = 0L
        var name = ""
        var lang = ""
        var location = ""
        var description = ""

        var first = true

        // Read in overweight data
        for (line <- Source.fromFile("src/main/resources/org/clulab/twitter4food/featureclassifier/overweight/overweightDataVerbose.txt").getLines){
            // Each account begins with its classification
            if ( (line equals "Overweight") || (line equals "Not overweight") ) {
                description = ""
                classification = line
                // Use the first iteration to fill in the variables with the first account's info
                // Otherwise add the filled account to the appropriate list
                if (!first) {
                    val account = new TwitterAccount(handle, id, name, lang, null, location, description, null)
                    // Add account to training
                    if (trainAccounts.length < trainPercent * (totalOverweight + totalNotOverweight)) {
                        trainAccounts = account :: trainAccounts
                        trainLabels = classification :: trainLabels
                    }
                    // Add to testing
                    else {
                        testAccounts = account :: testAccounts
                        testLabels = classification :: testLabels
                    }
                } else {
                    first = false
                }
            }
            // The tab-separated line will have meta data about account
            else if (line.split("\t").length > 3) {
                val attr = line.split("\t")
                handle = attr(0)
                id = attr(1).toLong
                name = attr(2)
                lang = attr(3)
                if (attr.length >= 5)
                    location = attr(4)
            }
            // Since descriptions can contain newlines, build the description until next account comes
            else {
                description += line
            }
        }
        // Add last account that was filled from loop
        if ( (classification equals "Overweight") || (classification equals "Not overweight") ) {
            val account = new TwitterAccount(handle, id, name, lang, null, location, description, null)
            // Add account to appropriate list
            if (trainAccounts.length < trainPercent * (totalOverweight + totalNotOverweight)) {
                trainAccounts = account :: trainAccounts
                trainLabels = classification :: trainLabels
            }
            else {
                testAccounts = account :: testAccounts
                testLabels = classification :: testLabels
            }
        }

        // Train and print results
        println("Training accounts...")
        oc.train(trainAccounts, trainLabels)

        println("Results:")
        println("Predicted\tActual")
        for (i <- testAccounts.indices) {
            println(s"${oc.classify(testAccounts(i))}\t${testLabels(i)}")
        }

    }
}