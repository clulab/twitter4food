package org.clulab.twitter4food.featureclassifier

import edu.arizona.sista.learning.{LinearSVMClassifier, RVFDataset}
import edu.arizona.sista.struct.Counter
import org.clulab.twitter4food.struct._

class GenderClassifier(
  val useUnigrams: Boolean = true,
  val useBigrams: Boolean = false,
  val useTopics: Boolean = false,
  val useDictionaries: Boolean = false,
  val useEmbeddings: Boolean = false) extends FeatureClassifier {


  val featureExtractor = new FeatureExtractor(useUnigrams, useBigrams, 
    useTopics, useDictionaries, useEmbeddings)
  val subClassifier = new LinearSVMClassifier[String, String]()
  var dataset = new RVFDataset[String, String]()

  override def train(accounts: Seq[TwitterAccount], labels: Seq[String]) = {
    // Clear current dataset if training on new one
    dataset = new RVFDataset[String, String]()
    // Populate dataset
    for (i <- accounts.indices) 
      dataset += featureExtractor.mkDatum(accounts(i), labels(i))
    subClassifier.train(dataset)
  }

  override def scoresOf(account: TwitterAccount): Counter[String] = {
    subClassifier.scoresOf(featureExtractor.mkDatum(account, "unknown"))
  }
}