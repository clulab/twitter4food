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

    // just for finding whether we're using US proportions or all accounts
    val params = Utils.parseArgs(args)
    val partitionFile = if (params.usProps)
      config.getString("classifiers.overweight.usFolds")
    else
      config.getString("classifiers.overweight.folds")

    val partitions = FileUtils.readFromCsv(partitionFile).map { user =>
      user(1).toLong -> user(0).toInt // id -> partition
    }.toMap

    val labeledAccts = FileUtils.loadTwitterAccounts(config.getString("classifiers.overweight.data"))
      .toSeq
      .filter(_._1.tweets.nonEmpty)
      .filter{ case (acct, lbl) => partitions.contains(acct.id)}

    val (accounts, labels) = labeledAccts.unzip

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
      useLocation = true,
      useTimeDate = true,
      useFoodPerc = true,
      useCaptions = true,
      useFollowers = true,
      useFollowees = true,
      useRT = true,
      useGender = true,
      useAge = true,
      useRace = true,
      useHuman = true,
      datumScaling = true
    )

    val predictions = oc.fscv(accounts,
      labels,
      partitions,
      Option(followers),
      Option(followees),
      Utils.svmFactory,
      Eval.f1ForLabel("Overweight")
    )


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