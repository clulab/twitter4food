package org.clulab.twitter4food.t2dm

import java.io.{BufferedWriter, FileWriter}
import java.nio.file.{Files, Paths}

import com.typesafe.config.ConfigFactory
import org.clulab.learning.L1LinearSVMClassifier
import org.clulab.twitter4food.featureclassifier.ClassifierImpl
import org.clulab.twitter4food.struct.TwitterAccount
import org.clulab.twitter4food.util.{Eval, FileUtils}
import org.slf4j.LoggerFactory

object OverweightSuite {
  def main(args: Array[String]) {
    val logger = LoggerFactory.getLogger(this.getClass)
    val config = ConfigFactory.load

    logger.info("Loading training accounts...")
    val train = FileUtils.load(config.getString("classifiers.overweight.trainingData"))
    logger.info("Loading dev accounts...")
    val dev = FileUtils.load(config.getString("classifiers.overweight.devData"))

    logger.info("Loading follower accounts...")
    val followers = ClassifierImpl.loadFollowers(train.keys.toSeq ++ dev.keys.toSeq)

    val oc = new OverweightClassifier(
      useUnigrams = true,
      useBigrams = true,
      useTopics = true,
      useDictionaries = true,
      useEmbeddings = true,
      useCosineSim = true,
      useFollowers = true,
      useFollowees = true,
      useGender = true,
      useRace = true,
      datumScaling = true
    )

    oc.featureSelectionIncremental(train ++ dev, followers)
  }
}