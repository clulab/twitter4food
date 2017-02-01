package org.clulab.twitter4food.t2dm

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.{Files, Paths}

import org.slf4j.LoggerFactory
import com.typesafe.config.ConfigFactory
import org.clulab.twitter4food.featureclassifier.{ClassifierImpl, Ensemble}
import org.clulab.twitter4food.util.{Eval, FileUtils, Utils}

import scala.util.Random

/**
  * A classifier for classifying a TwitterAccount as "Overweight" or "Not overweight".
  *
  * @author terron
  * @author Dane Bell
  */
class OverweightClassifier(
  useUnigrams: Boolean = false,
  useBigrams: Boolean = false,
  useName: Boolean = false,
  useTopics: Boolean = false,
  useDictionaries: Boolean = false,
  useAvgEmbeddings: Boolean = false,
  useMinEmbeddings: Boolean = false,
  useMaxEmbeddings: Boolean = false,
  useCosineSim: Boolean = false,
  useTimeDate: Boolean = false,
  useFollowers: Boolean = false,
  useFollowees: Boolean = false,
  useGender: Boolean = false,
  useRace: Boolean = false,
  useHuman: Boolean = false,
  dictOnly: Boolean = false,
  denoise: Boolean = false,
  datumScaling: Boolean = false,
  featureScaling: Boolean = false)
  extends ClassifierImpl(
    useUnigrams=useUnigrams,
    useBigrams=useBigrams,
    useName=useName,
    useTopics=useTopics,
    useDictionaries=useDictionaries,
    useAvgEmbeddings=useAvgEmbeddings,
    useMinEmbeddings=useMinEmbeddings,
    useMaxEmbeddings=useMaxEmbeddings,
    useCosineSim=useCosineSim,
    useTimeDate=useTimeDate,
    useFollowers=useFollowers,
    useFollowees=useFollowees,
    useGender=useGender,
    useRace=useRace,
    useHuman=useHuman,
    dictOnly=dictOnly,
    datumScaling=datumScaling,
    featureScaling=featureScaling,
    variable = "overweight") {
  val labels = Set("Overweight", "Not overweight")
}

object OverweightClassifier {
  import ClassifierImpl._

  val logger = LoggerFactory.getLogger(this.getClass)

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
      params.useFollowees
    )
    val default = allFeatures.forall(!_) // true if all features are off

    val portions = if (params.learningCurve) (1 to 20).map(_.toDouble / 20) else Seq(1.0)

    val nonFeatures = Seq("--analysis", "--test", "--learningCurve")
    // This model and results are specified by all input args that represent featuresets
    val fileExt = args.filterNot(nonFeatures.contains).sorted.mkString("").replace("-", "")

    val outputDir = config.getString("classifier") + "/overweight/results/" + fileExt
    if (!Files.exists(Paths.get(outputDir))) {
      if (new File(outputDir).mkdir()) logger.info(s"Created output directory $outputDir")
      else logger.info(s"ERROR: failed to create output directory $outputDir")
    }

    val modelFile = s"${config.getString("overweight")}/model/$fileExt.dat"
    // Instantiate classifier after prompts in case followers are being used (file takes a long time to load)

    logger.info("Loading Twitter accounts")
    val labeledAccts = FileUtils.load(config.getString("classifiers.overweight.data_raw"))
      .toSeq
      .filter(_._1.tweets.nonEmpty)

    // Scale number of accounts so that weights aren't too biased against Overweight
    val desiredProps = Map( "Overweight" -> 0.5, "Not overweight" -> 0.5 )
    val subsampled = Utils.subsample(labeledAccts, desiredProps)

    // Remove tweets that are spammy
    val denoised = subsampled.map{ case (acct, lbl) => Utils.denoise(acct) -> lbl }.filter(_._1.tweets.nonEmpty)

    val followers = if(params.useFollowers) {
      logger.info("Loading follower accounts...")
      Option(ClassifierImpl.loadFollowers(denoised.map(_._1)))
    } else None

    val followees = if(params.useFollowees) {
      logger.info("Loading followee accounts...")
      Option(ClassifierImpl.loadFollowees(denoised.map(_._1), "overweight"))
    } else None

    val evals = for {
      days <- Seq(7, 14, 30, 60, 90, 182, 365, 730, 10000)
    } yield {
      val (accts, lbls) = denoised.unzip

      // Convert java.util.Date into java.time.LocalDateTime
      val zid = java.time.ZoneId.of("GMT")

      val timeLim = for (acct <- accts) yield {
        val dateTimes = acct.tweets.map(t => t -> java.time.LocalDateTime.ofInstant(t.createdAt.toInstant, zid))
        val oldest = dateTimes.unzip._2.sortWith(_.compareTo(_) > 0).head minusDays days
        val newer = dateTimes.filter{ case (t, dt) => dt isAfter oldest }.unzip._1
        acct.copy(tweets = newer)
      }

      val numAllTweets = accts.map(_.tweets.length).sum.toDouble
      val numNewTweets = timeLim.map(_.tweets.length).sum.toDouble
      val portion = numNewTweets / numAllTweets * 100.0
      logger.info(f"$portion%1.3f%% of tweets are less than $days days old.")

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
        useFollowers = params.useFollowers,
        useFollowees = params.useFollowees,
        useGender = params.useGender,
        useRace = params.useRace,
        dictOnly = true,
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
        useFollowers = params.useFollowers,
        useFollowees = params.useFollowees,
        useGender = params.useGender,
        useRace = params.useRace,
        dictOnly = true,
        datumScaling = params.datumScaling,
        featureScaling = params.featureScaling)

      val ocs = new Ensemble(Seq(oc1, oc2))

      logger.info("Training classifiers...")
      val predictions = ocs.overweightCV(timeLim, lbls, followers, followees, Utils.svmFactory)

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

      (days, portion, predictions.length, precision, recall, macroAvg, microAvg)
    }

    println(s"\n$fileExt\n#days\t%train\t#accts\tp\tr\tf1\tf1(r*5)\tmacro\tmicro")
    evals.foreach { case (days, portion, numAccounts, precision, recall, macroAvg, microAvg) =>
      println(s"$days\t$portion\t$numAccounts\t$precision\t$recall\t${fMeasure(precision, recall, 1)}\t${fMeasure(precision, recall, .2)}" +
        s"\t$macroAvg\t$microAvg")
    }
  }
}