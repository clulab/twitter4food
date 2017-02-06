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

    val train = if (params.runOnTest) {
      val tr = FileUtils.load(config.getString("classifiers.overweight.trainingData")).toSeq
      val dv = FileUtils.load(config.getString("classifiers.overweight.devData")).toSeq
      tr ++ dv
    } else FileUtils.load(config.getString("classifiers.overweight.trainingData")).toSeq

    val test = if (params.runOnTest)
      FileUtils.load(config.getString("classifiers.overweight.testData")).toSeq
    else
      FileUtils.load(config.getString("classifiers.overweight.devData")).toSeq

    val (trainFollowers, testFollowers) = if(params.useFollowers) {
      logger.info("Loading follower accounts...")
      (Option(ClassifierImpl.loadFollowers(train.map(_._1))),
        Option(ClassifierImpl.loadFollowers(test.map(_._1))))
    } else (None, None)

    val (trainFollowees, testFollowees) = if(params.useFollowees) {
      logger.info("Loading followee accounts...")
      (Option(ClassifierImpl.loadFollowees(train.map(_._1), "overweight")),
        Option(ClassifierImpl.loadFollowees(test.map(_._1), "overweight")))
    } else (None, None)

    val (accts, lbls) = train.unzip
    // Convert java.util.Date into java.time.LocalDateTime
    val zid = java.time.ZoneId.of("GMT")

    val window = 0.10
    val stride = 0.5
    val classifiers = for {
      start <- 0.0 to 0.9 by stride
    } yield {

      val timeLim = for (acct <- accts) yield {
        val numTweets = acct.tweets.length.toDouble
        val first = (start * numTweets).toInt
        val last = ((start + window) * numTweets).toInt
        acct.copy(tweets = acct.tweets.slice(first, last))
      }

      val numAllTweets = accts.map(_.tweets.length).sum.toDouble
      val numNewTweets = timeLim.map(_.tweets.length).sum.toDouble
      val portion = numNewTweets / numAllTweets * 100.0
      logger.info(f"$portion%1.3f%% of tweets in $start%1.2f window.")

      val oc = new OverweightClassifier(
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
        datumScaling = params.datumScaling,
        featureScaling = params.featureScaling)

      logger.info("Training classifiers...")
      oc.train(accts, lbls, trainFollowers, trainFollowees)

      (start, accts.length, oc)
    }

    val evals = for {
      (portion, numAccounts, oc) <- classifiers
    } yield {
      // Set progress bar
      val pb = new me.tongfei.progressbar.ProgressBar("main()", 100)
      pb.start()
      pb.maxHint(test.size)
      pb.setExtraMessage(s"Evaluating on ${if(params.runOnTest) "test" else "dev"}...")

      // Classify accounts
      val testLabels = test.map(_._2)
      val predictedLabels = test.map(_._1).map { u =>
        pb.step()
        oc.classify(u)
      }

      pb.stop()

      // Print results
      val (evalMeasures, microAvg, macroAvg) = Eval.evaluate(testLabels, predictedLabels, test.map(_._1))

      val evalMetric = evalMeasures(oc.labels.toSeq.sorted.head)
      val precision = evalMetric.P
      val recall = evalMetric.R

      (portion, predictedLabels.length, precision, recall, macroAvg, microAvg)
    }

    println(s"\n$fileExt\n#days\t%train\t#accts\tp\tr\tf1\tf1(r*5)\tmacro\tmicro")
    evals.foreach { case (portion, numAccounts, precision, recall, macroAvg, microAvg) =>
      println(s"$portion\t$numAccounts\t$precision\t$recall\t${fMeasure(precision, recall, 1)}\t${fMeasure(precision, recall, .2)}" +
        s"\t$macroAvg\t$microAvg")
    }
  }
}