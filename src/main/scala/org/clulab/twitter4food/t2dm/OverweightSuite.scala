package org.clulab.twitter4food.t2dm

import com.typesafe.config.ConfigFactory
import org.clulab.twitter4food.featureclassifier.ClassifierImpl
import org.clulab.twitter4food.featureclassifier.ClassifierImpl._
import org.clulab.twitter4food.util.{Eval, FileUtils, Utils}
import org.slf4j.LoggerFactory

object OverweightSuite {
  def main(args: Array[String]) {
    val logger = LoggerFactory.getLogger(this.getClass)
    val config = ConfigFactory.load

    val toTrainOn = {
      logger.info("Loading training accounts...")
      val train = FileUtils.load(config.getString("classifiers.overweight.trainingData"))
      logger.info("Loading dev accounts...")
      val dev = FileUtils.load(config.getString("classifiers.overweight.devData"))
      train ++ dev
    }

    logger.info("Loading follower accounts...")
    val followers = ClassifierImpl.loadFollowers(toTrainOn.keys.toSeq)
    logger.info("Loading followee accounts...")
    val followees = ClassifierImpl.loadFollowees(toTrainOn.keys.toSeq, "overweight")

    val oc = new OverweightClassifier(
      useUnigrams = true,
      // useBigrams = true, // bigrams are costly and hurt performance
      useTopics = true,
      useDictionaries = true,
      useAvgEmbeddings = true,
      useMinEmbeddings = true,
      useMaxEmbeddings = true,
      useCosineSim = true,
      useFollowers = true,
      useFollowees = true,
      useGender = true,
      useRace = true,
      useHuman = true,
      datumScaling = true
    )

    val dataset = oc.featureSelectionIncremental(toTrainOn, followers, followees, Eval.f1ForLabel("Overweight"))

    val (predictions, avgWeights, falsePos, falseNeg) = oc.overweightCV(dataset, Utils.svmFactory)

    // Print results
    val (evalMeasures, microAvg, macroAvg) = Eval.evaluate(predictions)

    val evalMetric = if (evalMeasures.keySet contains "Overweight") {
      evalMeasures("Overweight")
    } else {
      logger.debug(s"Labels are {${evalMeasures.keys.mkString(", ")}}. Evaluating on ${evalMeasures.head._1}")
      evalMeasures.head._2
    }
    val precision = evalMetric.P
    val recall = evalMetric.R

    println(s"p\tr\tf1\tf1(r*5)\tmacro\tmicro")
    println(s"$precision\t$recall\t${fMeasure(precision, recall, 1)}\t${fMeasure(precision, recall, .2)}" +
        s"\t$macroAvg\t$microAvg")
  }
}