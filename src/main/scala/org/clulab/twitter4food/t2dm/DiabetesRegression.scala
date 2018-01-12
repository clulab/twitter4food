package org.clulab.twitter4food.t2dm

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.{Files, Paths}

import org.slf4j.LoggerFactory
import com.typesafe.config.ConfigFactory
import org.clulab.twitter4food.regression.RegressionImpl
import org.clulab.twitter4food.struct.TwitterAccount
import org.clulab.twitter4food.util.{BootstrapSignificance, Eval, FileUtils, Utils}

/**
  * A regression for guessing an [[TwitterAccount]]'s risk of developing diabetes
  *
  * @author terron
  * @author Dane Bell
  */
class DiabetesRegression(
                          useUnigrams: Boolean = false,
                          useBigrams: Boolean = false,
                          useName: Boolean = false,
                          useTopics: Boolean = false,
                          useDictionaries: Boolean = false,
                          useEmbeddings: Boolean = false,
//                          useAvgEmbeddings: Boolean = false,
//                          useMinEmbeddings: Boolean = false,
//                          useMaxEmbeddings: Boolean = false,
                          useCosineSim: Boolean = false,
                          useLocation: Boolean = false,
                          useTimeDate: Boolean = false,
//                          useFoodPerc: Boolean = false,
//                          useCaptions: Boolean = false,
                          useFollowers: Boolean = false,
                          useFollowees: Boolean = false,
                          useRT: Boolean = false,
                          useGender: Boolean = false,
                          useAge: Boolean = false,
//                          useRace: Boolean = false,
//                          useHuman: Boolean = false,
                          dictOnly: Boolean = false,
                          denoise: Boolean = false,
                          datumScaling: Boolean = false,
                          featureScaling: Boolean = false)
  extends RegressionImpl(
    useUnigrams=useUnigrams,
    useBigrams=useBigrams,
    useName=useName,
    useTopics=useTopics,
    useDictionaries=useDictionaries,
    useEmbeddings=useEmbeddings,
//    useAvgEmbeddings=useAvgEmbeddings,
//    useMinEmbeddings=useMinEmbeddings,
//    useMaxEmbeddings=useMaxEmbeddings,
    useCosineSim=useCosineSim,
    useLocation=useLocation,
    useTimeDate=useTimeDate,
//    useFoodPerc=useFoodPerc,
//    useCaptions=useCaptions,
    useFollowers=false,
    useFollowees=false,
    useRT=useRT,
    useGender=useGender,
    useAge=useAge,
//    useRace=useRace,
//    useHuman=useHuman,
    dictOnly=dictOnly,
    denoise=denoise,
    datumScaling=datumScaling,
    featureScaling=featureScaling,
    variable = "diabetes")

object DiabetesRegression {
  import org.clulab.twitter4food.regression.RegressionImpl._

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
      params.useEmbeddings,
//      params.useAvgEmbeddings,
//      params.useMinEmbeddings,
//      params.useMaxEmbeddings,
      params.useCosineSim,
      params.useLocation,
      params.useTimeDate,
//      params.useFoodPerc,
//      params.useCaptions,
      params.useFollowees
    )
    val default = allFeatures.forall(!_) // true if all features are off

    val portions = if (params.learningCurve) (1 to 20).map(_.toDouble / 20) else Seq(1.0)

    val nonFeatures = Seq("--analysis", "--test", "--learningCurve")
    // This model and results are specified by all input args that represent featuresets
    val fileExt = args.filterNot(nonFeatures.contains).sorted.mkString("").replace("-", "")

    val outputDir = config.getString("regression") + "/diabetes/results/" + fileExt
    if (!Files.exists(Paths.get(outputDir))) {
      if (new File(outputDir).mkdir()) logger.info(s"Created output directory $outputDir")
      else logger.info(s"ERROR: failed to create output directory $outputDir")
    }

    val modelFile = s"${config.getString("diabetes")}/model/$fileExt.dat"

    val partitionFile = config.getString("classifiers.diabetes.folds")
    val partitions = FileUtils.readFromCsv(partitionFile).map { user =>
      user(1).toLong -> user(0).toInt // id -> partition
    }.toMap

    logger.info("Loading Twitter accounts")
    val (accts, classLbls) = FileUtils.loadRVTwitterAccounts(config.getString("classifiers.diabetes.data"))
      .toSeq
      .filter(_._1.tweets.nonEmpty)
      .filter{ case (acct, lbl) => partitions.contains(acct.id)}
      .unzip

    val handleToLabel = FileUtils.readFromCsv(config.getString("classifiers.diabetes.regression_data"))
      .map(line => line.head -> line(1))
      .toMap

    val searchResults = for {
      acct <- accts
      safe = Utils.sanitizeHandle(acct.handle)
      if handleToLabel contains safe
    } yield acct -> handleToLabel(safe)

    val (foundAccts, lbls) = searchResults.unzip

    logger.info(s"Found regression labels for ${foundAccts.length}/${accts.length} accounts.")

    val followers: Option[Map[String, Seq[TwitterAccount]]] = None
    val followees: Option[Map[String, Seq[String]]] = None

    val evals = for {
      portion <- portions
    } yield {

      val dc = new DiabetesRegression(
        useUnigrams = default || params.useUnigrams,
        useBigrams = params.useBigrams,
        useName = params.useName,
        useTopics = params.useTopics,
        useDictionaries = params.useDictionaries,
        useEmbeddings = params.useEmbeddings,
//        useAvgEmbeddings = params.useAvgEmbeddings,
//        useMinEmbeddings = params.useMinEmbeddings,
//        useMaxEmbeddings = params.useMaxEmbeddings,
        useCosineSim = params.useCosineSim,
        useLocation = params.useLocation,
        useTimeDate = params.useTimeDate,
//        useFoodPerc = params.useFoodPerc,
//        useCaptions= params.useCaptions,
        useFollowers = params.useFollowers,
        useFollowees = params.useFollowees,
        useRT = params.useRT,
        useGender = params.useGender,
        // useRace = params.useRace,
        dictOnly = params.dictOnly,
        denoise = params.denoise,
        datumScaling = params.datumScaling,
        featureScaling = params.featureScaling)

      logger.info("Training regression...")

      val (predictions, avgWeights) =
        dc.cv(
          foundAccts,
          lbls,
          partitions,
          portion,
          followers,
          followees,
          Utils.svmRegressionFactory
        )

      // Print results
      val (rsquared, rmse) = Eval.evaluate(predictions)

      // Write analysis only on full portion
      if (portion == 1.0) {
        if (params.fpnAnalysis) {
          // Perform analysis on false negatives and false positives
          outputAnalysis(outputDir, avgWeights)
        }

        // Save results
        val writer = new BufferedWriter(new FileWriter(outputDir + "/analysisMetrics.txt", false))
        writer.write(s"rsquared: $rsquared\n")
        writer.write(s"rmse: $rmse\n")
        writer.close()

        // Save individual predictions for bootstrap significance
        val predWriter = new BufferedWriter(new FileWriter(outputDir + "/predicted.txt", false))
        predWriter.write(s"gold\tpred\n")
        predictions.foreach(acct => predWriter.write(s"${acct._1}\t${acct._2}\n"))
        predWriter.close()
      }

      val (gold, pred) = predictions.unzip

      val sig = BootstrapSignificance.regressionBss(gold, pred)

      (portion, predictions.length, rsquared, rmse, sig)
    }

    println(s"\n$fileExt\n%train\t#accts\trsquared\trmse\tp-value")
    evals.foreach { case (portion, numAccounts, rsquared, rmse, sig) =>
      println(f"$portion\t$numAccounts\t$rsquared%1.5f\t$rmse%1.5f\t$sig%1.5f")
    }
  }
}