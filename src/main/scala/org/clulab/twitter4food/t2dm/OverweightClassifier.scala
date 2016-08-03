package org.clulab.twitter4food.t2dm

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.{Files, Paths}
import org.slf4j.LoggerFactory

import com.typesafe.config.ConfigFactory
import org.clulab.learning.{L1LinearSVMClassifier, LiblinearClassifier}
import org.clulab.twitter4food.featureclassifier.ClassifierImpl
import org.clulab.twitter4food.struct.TwitterAccount
import org.clulab.twitter4food.util.{Eval, FileUtils, TestUtils}

/**
  * Created by Terron on 2/15/16.
  *
  * A classifier for classifying a TwitterAccount as "Overweight" or "Not overweight".
  *
  * All parameters are consistent with those in FeatureExtractor
  */
class OverweightClassifier(
  useUnigrams: Boolean = false,
  useBigrams: Boolean = false,
  useTopics: Boolean = false,
  useDictionaries: Boolean = false,
  useEmbeddings: Boolean = false,
  useCosineSim: Boolean = false,
  useFollowers: Boolean = false,
  useFollowees: Boolean = false,
  useGender: Boolean = false,
  useRace: Boolean = false,
  datumScaling: Boolean = false,
  featureScaling: Boolean = false)
  extends ClassifierImpl(
    useUnigrams=useUnigrams,
    useBigrams=useBigrams,
    useTopics=useTopics,
    useDictionaries=useDictionaries,
    useEmbeddings=useEmbeddings,
    useCosineSim=useCosineSim,
    useFollowers=useFollowers,
    useFollowees=useFollowees,
    useGender=useGender,
    useRace=useRace,
    datumScaling=datumScaling,
    featureScaling=featureScaling,
    variable = "overweight")

object OverweightClassifier {

  val logger = LoggerFactory.getLogger(this.getClass)

  def main(args: Array[String]) {
    // Parse args using standard Config
    val params = TestUtils.parseArgs(args)
    val config = ConfigFactory.load

    // List of features (not counting domain adaptation)
    // if these are all false, set default to true to use unigrams anyway
    val allFeatures = Seq(
      params.useUnigrams,
      params.useBigrams,
      params.useTopics,
      params.useDictionaries,
      params.useEmbeddings,
      params.useCosineSim,
      params.useFollowees
    )
    val default = allFeatures.forall(!_) // true if all features are off

    val portions = if (params.learningCurve) Seq(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0) else Seq(1.0)

    val nonFeatures = Seq("--analysis", "--test", "--noTraining", "--learningCurve")
    // This model and results are specified by all input args that represent featuresets
    val fileExt = args.filterNot(nonFeatures.contains).sorted.mkString("").replace("-", "")

    val outputDir = config.getString("classifier") + "/overweight/results/" + fileExt
    if (!Files.exists(Paths.get(outputDir))) {
      if (new File(outputDir).mkdir()) logger.info(s"Created output directory $outputDir")
      else logger.info(s"ERROR: failed to create output directory $outputDir")
    }

    val modelFile = s"${config.getString("overweight")}/model/${fileExt}.dat"
    // Instantiate classifier after prompts in case followers are being used (file takes a long time to load)

    val classifiers = {
      if (params.noTraining && params.learningCurve) logger.warn("Learning curve requested, so not loading model from file...")
      if (params.noTraining && !params.learningCurve && Files.exists(Paths.get(modelFile))) {
        val oc = new OverweightClassifier(
          useUnigrams = default || params.useUnigrams,
          useBigrams = params.useBigrams,
          useTopics = params.useTopics,
          useDictionaries = params.useDictionaries,
          useEmbeddings = params.useEmbeddings,
          useCosineSim = params.useCosineSim,
          useFollowers = params.useFollowers,
          useFollowees = params.useFollowees,
          useGender = params.useGender,
          useRace = params.useRace,
          datumScaling = params.datumScaling,
          featureScaling = params.featureScaling)

        logger.info("Loading model from file...")
        val cl = LiblinearClassifier.loadFrom[String, String](modelFile)
        oc.subClassifier = Some(cl)
        Seq((1.0, 0, oc))
      } else {
        val toTrainOn = if (params.runOnTest) {
          logger.info("Loading training accounts...")
          val trainData = FileUtils.load(config.getString("classifiers.overweight.trainingData")).toSeq
          logger.info("Loading dev accounts...")
          val devData = FileUtils.load(config.getString("classifiers.overweight.devData")).toSeq
          trainData ++ devData
        } else {
          logger.info("Loading training accounts...")
          FileUtils.load(config.getString("classifiers.overweight.trainingData")).toSeq
        }

        val followers = Option(ClassifierImpl.loadFollowers(toTrainOn.map(_._1)))

        for {
          portion <- portions
          maxIndex = (portion * toTrainOn.length).toInt
        } yield {
          val (trainAccounts, trainLabels) = toTrainOn.slice(0, maxIndex).unzip

          val oc = new OverweightClassifier(
            useUnigrams = default || params.useUnigrams,
            useBigrams = params.useBigrams,
            useTopics = params.useTopics,
            useDictionaries = params.useDictionaries,
            useEmbeddings = params.useEmbeddings,
            useCosineSim = params.useCosineSim,
            useFollowers = params.useFollowers,
            useFollowees = params.useFollowees,
            useGender = params.useGender,
            useRace = params.useRace,
            datumScaling = params.datumScaling,
            featureScaling = params.featureScaling)

          logger.info("Training classifier...")
          oc.setClassifier(new L1LinearSVMClassifier[String, String]())
          oc.train(trainAccounts, followers, trainLabels)
          // Only save models using full training
          if (maxIndex == toTrainOn.length) oc.subClassifier.get.saveTo(modelFile)

          (portion, maxIndex, oc)
        }
      }
    }
    val toTestOn = if (params.runOnTest) {
      logger.info("Loading test accounts...")
      FileUtils.load(config.getString("classifiers.overweight.testData"))
    } else {
      logger.info("Loading dev accounts...")
      FileUtils.load(config.getString("classifiers.overweight.devData"))
    }

    val evals = for ((portion, numAccounts, oc) <- classifiers) yield {

      // Set progress bar
      val pb = new me.tongfei.progressbar.ProgressBar("main()", 100)
      pb.start()
      pb.maxHint(toTestOn.size)
      pb.setExtraMessage("Testing on dev accounts...")

      // Classify accounts
      val testSetLabels = toTestOn.values.toSeq
      val predictedLabels = toTestOn.keys.toSeq.map { u =>
        pb.step()
        oc.classify(u)
      }

      pb.stop()

      // Print results
      val (evalMeasures, microAvg, macroAvg) = Eval.evaluate(testSetLabels, predictedLabels, toTestOn.keys.toSeq)

      val evalMetric = evalMeasures("Overweight")
      val precision = evalMetric.P
      val recall = evalMetric.R

      if (portion == 1.0) {
        if (params.fpnAnalysis & oc.subClassifier.nonEmpty) {
          // Perform analysis on false negatives and false positives
          println("False negatives:")
          evalMetric.FNAccounts.foreach(account => print(account.handle + "\t"))
          println("\n====")
          outputAnalysis(outputDir + "/analysisFN.txt", "*** False negatives ***\n\n", evalMetric.FNAccounts, oc)

          println("False positives:")
          evalMetric.FPAccounts.foreach(account => print(account.handle + "\t"))
          println("\n====")
          outputAnalysis(outputDir + "/analysisFP.txt", "*** False positives ***\n\n", evalMetric.FPAccounts, oc)
        }

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
        val predicted = new BufferedWriter(new FileWriter(outputDir + "/predicted.txt", false))
        predicted.write(s"gold\tpred\n")
        testSetLabels.zip(predictedLabels).foreach(acct => predicted.write(s"${acct._1}\t${acct._2}\n"))
        predicted.close()
      }

      (portion, numAccounts, precision, recall, macroAvg, microAvg)
    }

    println("\n%train\t#accts\tp\tr\tf1\tf1(r*5)\tmacro\tmicro")
    evals.foreach { case (portion, numAccounts, precision, recall, macroAvg, microAvg) =>
      println(s"$portion\t$numAccounts\t$precision\t$recall\t${fMeasure(precision, recall, 1)}\t${fMeasure(precision, recall, .2)}" +
        s"\t$macroAvg\t$microAvg")
    }
  }

  private def outputAnalysis(outputFile:String, header:String, accounts: Seq[TwitterAccount], oc: OverweightClassifier): Unit = {
    // Set progress bar
    var numAccountsToPrint = 20
    val numWeightsToPrint = 30
    val pb = new me.tongfei.progressbar.ProgressBar("outputAnalysis()", 100)
    pb.start()
    pb.maxHint(numAccountsToPrint)
    pb.setExtraMessage(header)

    // Initialize writer
    val writer = new BufferedWriter(new FileWriter(outputFile, false))
    var isFirst = true
    writer.write(header)

    // Iterate over accounts
    for (account <- accounts) {
      if (numAccountsToPrint > 0) {
        // Analyze account
        val (topWeights, dotProduct) = TestUtils.analyze(oc.subClassifier.get, Set("Overweight", "Not overweight"),
          account, oc.featureExtractor)
        // Only print the general weights on the features once
        if (isFirst) {
          for ((label, sequence) <- topWeights) {
            writer.write(s"Top weights for $label:\n")
            var numToPrint = numWeightsToPrint
            for ((feature, score) <- sequence) {
              if ((numToPrint > 0) && (score > 0.0)) {
                writer.write(s"$feature -> $score\n")
                numToPrint = numToPrint - 1
              }
            }
            writer.write("================================\n")
          }
          isFirst = false
        }
        // Print hadamard product for every account
        writer.write(s"Hadamard product for ${account.handle}:\n")
        for ((label, sequence) <- dotProduct) {
          if (label equals "Overweight") {
            var numToPrint = numWeightsToPrint
            for ((feature, score) <- sequence) {
              if ((numToPrint > 0) && (score > 0.0)) {
                writer.write(s"$feature -> $score\n")
                numToPrint = numToPrint - 1
              }
            }
          }
        }
        writer.write("================================\n")
      }
      pb.step()
      numAccountsToPrint -= 1
    }
    writer.close
    pb.stop()
  }

  def fMeasure(precision: Double, recall: Double, beta: Double): Double =
    (1 + Math.pow(beta, 2)) * ((precision * recall) / (Math.pow(beta, 2) * precision + recall))
}