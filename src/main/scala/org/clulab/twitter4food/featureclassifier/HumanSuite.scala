package org.clulab.twitter4food.featureclassifier

import com.typesafe.config.ConfigFactory
import org.clulab.twitter4food.util.{Eval, FileUtils}
import org.slf4j.LoggerFactory

object HumanSuite {
  def main(args: Array[String]) {
    val logger = LoggerFactory.getLogger(this.getClass)
    val config = ConfigFactory.load

    logger.info("Loading training accounts...")
    val train = FileUtils.load(config.getString("classifiers.human.trainingData"))
    logger.info("Loading dev accounts...")
    val dev = FileUtils.load(config.getString("classifiers.human.devData"))

    logger.info("Loading follower accounts...")
    val followers = ClassifierImpl.loadFollowers(train.keys.toSeq ++ dev.keys.toSeq)

    val hc = new HumanClassifier(
      useUnigrams = true,
      useBigrams = true,
      useTopics = true,
      useDictionaries = true,
      useEmbeddings = true,
      useCosineSim = true,
      useFollowers = true,
      useFollowees = true,
      useRace = true,
      datumScaling = true
    )

    hc.featureSelectionIncremental(train ++ dev, followers, Eval.microOnly)
  }
}