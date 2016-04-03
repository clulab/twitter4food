package org.clulab.twitter4food.featureclassifier

import edu.arizona.sista.learning._
import edu.arizona.sista.struct.Counter
import org.clulab.twitter4food.struct._
import org.clulab.twitter4food.util._

class ClassifierImpl(
  val useUnigrams: Boolean = true,
  val useBigrams: Boolean = false,
  val useTopics: Boolean = false,
  val useDictionaries: Boolean = false,
  val useEmbeddings: Boolean = false) extends FeatureClassifier {


  val featureExtractor = new FeatureExtractor(useUnigrams, useBigrams, 
    useTopics, useDictionaries, useEmbeddings)
  var subClassifier: Option[LiblinearClassifier[String, String]] = None
  var dataset = new RVFDataset[String, String]()

  override def train(accounts: Seq[TwitterAccount], labels: Seq[String]) = {
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
    subClassifier.get.train(dataset)
  }

  override def scoresOf(account: TwitterAccount): Counter[String] = {
    if(subClassifier.isDefined) {
      subClassifier.get.scoresOf(featureExtractor.mkDatum(account, "unknown"))
      } else throw new RuntimeException("ERROR: must train before using scoresOf!")
  }
}