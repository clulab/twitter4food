package org.clulab.twitter4food.t2dm

import edu.arizona.sista.learning.{LinearSVMClassifier, RVFDataset}
import edu.arizona.sista.struct.Counter
import org.clulab.twitter4food.featureclassifier.FeatureClassifier
import org.clulab.twitter4food.struct.{FeatureExtractor, TwitterAccount}

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
    def main(args: Array[String]) {
        val oc = new OverweightClassifier()

    }
}