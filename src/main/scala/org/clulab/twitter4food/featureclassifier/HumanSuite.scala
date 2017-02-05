package org.clulab.twitter4food.featureclassifier

import com.typesafe.config.ConfigFactory
import org.clulab.twitter4food.struct.TwitterAccount
import org.clulab.twitter4food.util.{Eval, FileUtils}
import org.slf4j.LoggerFactory

object HumanSuite {
  def main(args: Array[String]) {
    val logger = LoggerFactory.getLogger(this.getClass)
    val config = ConfigFactory.load

    val toTrainOn = {
      logger.info("Loading training accounts...")
      val train = FileUtils.load(config.getString("classifiers.human.trainingData"))
      logger.info("Loading dev accounts...")
      val dev = FileUtils.load(config.getString("classifiers.human.devData"))
      train ++ dev
    }

    logger.info("Loading follower accounts...")
    val followers = Map[String, Seq[TwitterAccount]]() //ClassifierImpl.loadFollowers(toTrainOn.keys.toSeq)
    logger.info("Loading followee accounts...")
    val followees = ClassifierImpl.loadFollowees(toTrainOn.keys.toSeq, "human")

    val hc = new HumanClassifier(
      useUnigrams = true,
      //useBigrams = true, // bigrams are costly and hurt performance
      useName = true,
      useTopics = true,
      useDictionaries = true,
      useAvgEmbeddings = true,
      useMinEmbeddings = true,
      useMaxEmbeddings = true,
      useCosineSim = true,
      useTimeDate = true,
      // useFollowers = true, // we don't currently have their followers
      useFollowees = true,
      useRT=true,
      datumScaling = true,
      customFeatures = HumanClassifier.customFeatures
    )

    hc.featureSelectionIncremental(toTrainOn, followers, followees, Eval.microOnly)
  }
}