package org.clulab.twitter4food.t2dm

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.{Files, Paths}

import org.slf4j.LoggerFactory
import com.typesafe.config.ConfigFactory
import org.clulab.twitter4food.featureclassifier.ClassifierImpl
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
  useTopics: Boolean = false,
  useDictionaries: Boolean = false,
  useAvgEmbeddings: Boolean = false,
  useMinEmbeddings: Boolean = false,
  useMaxEmbeddings: Boolean = false,
  useCosineSim: Boolean = false,
  useFollowers: Boolean = false,
  useFollowees: Boolean = false,
  useGender: Boolean = false,
  useRace: Boolean = false,
  useHuman: Boolean = false,
  datumScaling: Boolean = false,
  featureScaling: Boolean = false)
  extends ClassifierImpl(
    useUnigrams=useUnigrams,
    useBigrams=useBigrams,
    useTopics=useTopics,
    useDictionaries=useDictionaries,
    useAvgEmbeddings=useAvgEmbeddings,
    useMinEmbeddings=useMinEmbeddings,
    useMaxEmbeddings=useMaxEmbeddings,
    useCosineSim=useCosineSim,
    useFollowers=useFollowers,
    useFollowees=useFollowees,
    useGender=useGender,
    useRace=useRace,
    useHuman=useHuman,
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
      params.useTopics,
      params.useDictionaries,
      params.useAvgEmbeddings,
      params.useMinEmbeddings,
      params.useMaxEmbeddings,
      params.useCosineSim,
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
    val labeledAccts = FileUtils.load(config.getString("classifiers.overweight.data")).toSeq

    // Scale number of accounts so that weights aren't too biased against Overweight
    val desiredProps = Map( "Overweight" -> 0.5, "Not overweight" -> 0.5 )
    val subsampled = Utils.subsample(labeledAccts, desiredProps)

    val followers = if(params.useFollowers) Option(ClassifierImpl.loadFollowers(subsampled.map(_._1))) else None
    val followees = if(params.useFollowees) Option(ClassifierImpl.loadFollowees(subsampled.map(_._1), "overweight")) else None

    val evals = for {
      portion <- portions
      maxIndex = (portion * subsampled.length).toInt
    } yield {
      val (accts, lbls) = subsampled.slice(0, maxIndex).unzip

      val oc = new OverweightClassifier(
        useUnigrams = default || params.useUnigrams,
        useBigrams = params.useBigrams,
        useTopics = params.useTopics,
        useDictionaries = params.useDictionaries,
        useAvgEmbeddings = params.useAvgEmbeddings,
        useMinEmbeddings = params.useMinEmbeddings,
        useMaxEmbeddings = params.useMaxEmbeddings,
        useCosineSim = params.useCosineSim,
        useFollowers = params.useFollowers,
        useFollowees = params.useFollowees,
        useGender = params.useGender,
        useRace = params.useRace,
        datumScaling = params.datumScaling,
        featureScaling = params.featureScaling)

      logger.info("Training classifier...")

      val dataset = oc.constructDataset(accts, lbls, followers, followees)
      val predictions = oc.stratifiedCrossValidate[String, String](dataset, Utils.svmFactory)

      // Print results
      // val (evalMeasures, microAvg, macroAvg) = Eval.evaluate(gold, pred, accts)
      val (evalMeasures, microAvg, macroAvg) = Eval.evaluate(predictions)

      val evalMetric = if (evalMeasures.keySet contains "Overweight") {
        evalMeasures("Overweight")
      } else {
        logger.debug(s"Labels are {${evalMeasures.keys.mkString(", ")}}. Evaluating on ${evalMeasures.head._1}")
        evalMeasures.head._2
      }
      val precision = evalMetric.P
      val recall = evalMetric.R

      // Write analysis only on full portion
      if (portion == 1.0) {
//        if (params.fpnAnalysis & oc.subClassifier.nonEmpty &
//          (evalMetric.FNAccounts.nonEmpty || evalMetric.FPAccounts.nonEmpty)) {
//          // Perform analysis on false negatives and false positives
//          println("False negatives:")
//          evalMetric.FNAccounts.foreach(account => print(account.handle + "\t"))
//          println("\n====")
//          outputAnalysis(outputDir + "/analysisFN.txt", "*** False negatives ***\n\n", evalMetric.FNAccounts, oc, oc.labels)
//
//          println("False positives:")
//          evalMetric.FPAccounts.foreach(account => print(account.handle + "\t"))
//          println("\n====")
//          outputAnalysis(outputDir + "/analysisFP.txt", "*** False positives ***\n\n", evalMetric.FPAccounts, oc, oc.labels)
//        }

        // Save results
        val writer = new BufferedWriter(new FileWriter(outputDir + "/analysisMetrics.txt", false))
        writer.write(s"Precision: $precision\n")
        writer.write(s"Recall: $recall\n")
        writer.write(s"F-measure (harmonic mean): ${fMeasure(precision, recall, 1)}\n")
        writer.write(s"F-measure (recall 5x): ${fMeasure(precision, recall, .2)}\n")
        writer.write(s"Macro average: $macroAvg\n")
        writer.write(s"Micro average: $microAvg\n")
        writer.close()

        // Save individual predictions for bootstrap significance
        val predWriter = new BufferedWriter(new FileWriter(outputDir + "/predicted.txt", false))
        predWriter.write(s"gold\tpred\n")
        predictions.foreach(acct => predWriter.write(s"${acct._1}\t${acct._2}\n"))
        predWriter.close()
      }

      (portion, predictions.length, precision, recall, macroAvg, microAvg)
    }

    println(s"\n$fileExt\n%train\t#accts\tp\tr\tf1\tf1(r*5)\tmacro\tmicro")
    evals.foreach { case (portion, numAccounts, precision, recall, macroAvg, microAvg) =>
      println(s"$portion\t$numAccounts\t$precision\t$recall\t${fMeasure(precision, recall, 1)}\t${fMeasure(precision, recall, .2)}" +
        s"\t$macroAvg\t$microAvg")
    }
  }
}