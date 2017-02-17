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

    val labeledAccts = FileUtils.load(config.getString("classifiers.overweight.data")).toSeq

    // Scale number of accounts so that weights aren't too biased against Overweight
    val desiredProps = Map( "Overweight" -> 0.5, "Not overweight" -> 0.5 )
    val subsampled = Utils.subsample(labeledAccts, desiredProps)
    val (accounts, labels) = subsampled.unzip

    logger.info("Loading follower accounts...")
    val followers = ClassifierImpl.loadFollowers(accounts)
    logger.info("Loading followee accounts...")
    val followees = ClassifierImpl.loadFollowees(accounts, "overweight")

    val oc = new OverweightClassifier(
      useUnigrams = true,
      // useBigrams = true, // bigrams are costly and hurt performance
      useName = true,
      useTopics = true,
      useDictionaries = true,
      useAvgEmbeddings = true,
      useMinEmbeddings = true,
      useMaxEmbeddings = true,
      useCosineSim = true,
      useTimeDate = true,
      useFollowers = true,
      useFollowees = true,
      useRT = true,
      useGender = true,
      useAge = true,
      useRace = true,
      useHuman = true,
      datumScaling = true
    )

    val dataset = oc.constructDataset(accounts, labels, Option(followers), Option(followees))

    val predictions = oc.fscv(dataset, Utils.svmFactory, Eval.f1ForLabel("Overweight"))

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