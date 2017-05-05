package org.clulab.twitter4food.t2dm

import java.io.File
import java.nio.file.{Files, Paths}

import org.slf4j.{Logger, LoggerFactory}
import com.typesafe.config.ConfigFactory
import org.clulab.twitter4food.featureclassifier.{ClassifierImpl, Ensemble}
import org.clulab.twitter4food.util.{Eval, FileUtils, Utils}


object OverweightEnsemble {
  import ClassifierImpl._

  val logger: Logger = LoggerFactory.getLogger(this.getClass)

  def main(args: Array[String]) {
    // Parse args using standard Config
    val params = Utils.parseArgs(args)
    val config = ConfigFactory.load

    // List of features (not counting domain adaptation)
    // if these are all false, set default to true to use unigrams anyway
    val allFeatures = Seq(
      params.useUnigrams,
      params.useBigrams,
      params.useName,
      params.useTopics,
      params.useDictionaries,
      params.useAvgEmbeddings,
      params.useMinEmbeddings,
      params.useMaxEmbeddings,
      params.useCosineSim,
      params.useTimeDate,
      params.useFoodPerc,
      params.useFollowees
    )
    val default = allFeatures.forall(!_) // true if all features are off

    val portions = if (params.learningCurve) (1 to 20).map(_.toDouble / 20) else Seq(1.0)

    val nonFeatures = Seq("--analysis", "--test", "--learningCurve")
    // This model and results are specified by all input args that represent featuresets
    val fileExt = args.filterNot(nonFeatures.contains).sorted.mkString("").replace("-", "") + "Ensemble"

    val outputDir = config.getString("classifier") + "/overweight/results/" + fileExt
    if (!Files.exists(Paths.get(outputDir))) {
      if (new File(outputDir).mkdir()) logger.info(s"Created output directory $outputDir")
      else logger.info(s"ERROR: failed to create output directory $outputDir")
    }

    val modelFile = s"${config.getString("overweight")}/model/$fileExt.dat"

    val partitionFile = if (params.usProps)
      config.getString("classifiers.overweight.usFolds")
    else
      config.getString("classifiers.overweight.folds")

    val partitions = FileUtils.readFromCsv(partitionFile).map { user =>
      user(1).toLong -> user(0).toInt // id -> partition
    }.toMap

    logger.info("Loading Twitter accounts")
    val labeledAccts = FileUtils.loadTwitterAccounts(config.getString("classifiers.overweight.data_raw"))
      .toSeq
      .filter(_._1.tweets.nonEmpty)
      .filter{ case (acct, lbl) => partitions.contains(acct.id)}

    val followers = if(params.useFollowers) {
      logger.info("Loading follower accounts...")
      Option(ClassifierImpl.loadFollowers(labeledAccts.map(_._1)))
    } else None

    val followees = if(params.useFollowees) {
      logger.info("Loading followee accounts...")
      Option(ClassifierImpl.loadFollowees(labeledAccts.map(_._1), "overweight"))
    } else None

    val evals = for {
      portion <- portions
    } yield {
      val (accts, lbls) = labeledAccts.unzip

      val oc1 = new OverweightClassifier(
        useUnigrams = default || params.useUnigrams,
        useBigrams = params.useBigrams,
        useName = params.useName,
        useTopics = params.useTopics,
        useDictionaries = params.useDictionaries,
        useAvgEmbeddings = params.useAvgEmbeddings,
        useMinEmbeddings = params.useMinEmbeddings,
        useMaxEmbeddings = params.useMaxEmbeddings,
        useCosineSim = params.useCosineSim,
        useTimeDate = params.useTimeDate,
        useFoodPerc = params.useFoodPerc,
        useCaptions = params.useCaptions,
        useFollowers = params.useFollowers,
        useFollowees = params.useFollowees,
        useRT = params.useRT,
        useGender = params.useGender,
        useAge = params.useAge,
        useRace = params.useRace,
        dictOnly = true,
        denoise = false,
        datumScaling = params.datumScaling,
        featureScaling = params.featureScaling)

      val oc2 = new OverweightClassifier(
        useUnigrams = default || params.useUnigrams,
        useBigrams = params.useBigrams,
        useName = params.useName,
        useTopics = params.useTopics,
        useDictionaries = params.useDictionaries,
        useAvgEmbeddings = params.useAvgEmbeddings,
        useMinEmbeddings = params.useMinEmbeddings,
        useMaxEmbeddings = params.useMaxEmbeddings,
        useCosineSim = params.useCosineSim,
        useTimeDate = params.useTimeDate,
        useFoodPerc = params.useFoodPerc,
        useCaptions = params.useCaptions,
        useFollowers = params.useFollowers,
        useFollowees = params.useFollowees,
        useRT = params.useRT,
        useGender = params.useGender,
        useAge = params.useAge,
        useRace = params.useRace,
        dictOnly = false,
        denoise = true,
        datumScaling = params.datumScaling,
        featureScaling = params.featureScaling)

      val ocs = new Ensemble(Seq(oc1, oc2))

      logger.info("Training classifiers...")
      val predictions = ocs.overweightCV(accts, lbls, partitions, portion, followers, followees, Utils.svmFactory)

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

      (portion, predictions.length, precision, recall, macroAvg, microAvg)
    }

    println(s"\n$fileExt\n%train\t#accts\tp\tr\tf1\tf1(r*5)\tmacro\tmicro")
    evals.foreach { case (portion, numAccounts, precision, recall, macroAvg, microAvg) =>
      println(s"$portion\t$numAccounts\t$precision\t$recall\t${fMeasure(precision, recall, 1)}\t${fMeasure(precision, recall, .2)}" +
        s"\t$macroAvg\t$microAvg")
    }
  }
}