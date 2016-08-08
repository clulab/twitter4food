package org.clulab.twitter4food.t2dm

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.{Files, Paths}
import org.slf4j.LoggerFactory

import com.typesafe.config.ConfigFactory
import org.clulab.learning.{L1LinearSVMClassifier, LiblinearClassifier}
import org.clulab.twitter4food.featureclassifier.ClassifierImpl
import org.clulab.twitter4food.struct.TwitterAccount
import org.clulab.twitter4food.util.{Eval, FileUtils, Utils}

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

    val nonFeatures = Seq("--analysis", "--test", "--noTraining", "--learningCurve")
    // This model and results are specified by all input args that represent featuresets
    val fileExt = args.filterNot(nonFeatures.contains).sorted.mkString("").replace("-", "")

    val outputDir = config.getString("classifier") + "/overweight/results/" + fileExt
    if (!Files.exists(Paths.get(outputDir))) {
      if (new File(outputDir).mkdir()) logger.info(s"Created output directory $outputDir")
      else logger.info(s"ERROR: failed to create output directory $outputDir")
    }

    val modelFile = s"${config.getString("overweight")}/model/$fileExt.dat"
    // Instantiate classifier after prompts in case followers are being used (file takes a long time to load)

    val classifiers = {
      if (params.noTraining && params.learningCurve) logger.warn("Learning curve requested, so not loading model from file...")
      if (params.noTraining && !params.learningCurve && Files.exists(Paths.get(modelFile))) {
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

        val followers = if(params.useFollowers) Option(ClassifierImpl.loadFollowers(toTrainOn.map(_._1))) else None
        val followees = if(params.useFollowers) Option(ClassifierImpl.loadFollowees(toTrainOn.map(_._1), "overweight")) else None

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
          oc.setClassifier(new L1LinearSVMClassifier[String, String]())
          oc.train(trainAccounts, trainLabels, followers, followees)
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
        if (params.fpnAnalysis & oc.subClassifier.nonEmpty &
          (evalMetric.FNAccounts.nonEmpty || evalMetric.FPAccounts.nonEmpty)) {
          // Perform analysis on false negatives and false positives
          println("False negatives:")
          evalMetric.FNAccounts.foreach(account => print(account.handle + "\t"))
          println("\n====")
          outputAnalysis(outputDir + "/analysisFN.txt", "*** False negatives ***\n\n", evalMetric.FNAccounts, oc, oc.labels)

          println("False positives:")
          evalMetric.FPAccounts.foreach(account => print(account.handle + "\t"))
          println("\n====")
          outputAnalysis(outputDir + "/analysisFP.txt", "*** False positives ***\n\n", evalMetric.FPAccounts, oc, oc.labels)
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

    println(s"\n$fileExt\n%train\t#accts\tp\tr\tf1\tf1(r*5)\tmacro\tmicro")
    evals.foreach { case (portion, numAccounts, precision, recall, macroAvg, microAvg) =>
      println(s"$portion\t$numAccounts\t$precision\t$recall\t${fMeasure(precision, recall, 1)}\t${fMeasure(precision, recall, .2)}" +
        s"\t$macroAvg\t$microAvg")
    }
  }
}