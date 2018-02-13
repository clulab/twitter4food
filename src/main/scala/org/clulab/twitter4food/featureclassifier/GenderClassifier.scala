package org.clulab.twitter4food.featureclassifier

import org.clulab.learning._
import org.clulab.twitter4food.util._
import java.io._
import java.nio.file.{Files, Paths}

import com.typesafe.config.ConfigFactory
import org.slf4j.LoggerFactory

/** Gender classifier that predicts if a given twitter account is M (male)
  * or F (female)
  *
  * @author adikou
  * @date 03-13-2016
  */
class GenderClassifier(
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
  useRT: Boolean = false,
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
    useLocation=false,
    useTimeDate=useTimeDate,
    useFoodPerc=false,
    useCaptions=false,
    useFollowers=useFollowers,
    useFollowees=useFollowees,
    useRT=useRT,
    useGender=false,
    useAge=false,
    useRace=false,
    useHuman=false,
    dictOnly=false,
    denoise=false,
    datumScaling=datumScaling,
    featureScaling=featureScaling,
    variable = "gender"
  ) {
  val labels = Set("F", "M")
}

object GenderClassifier {

  import ClassifierImpl._

  def main(args: Array[String]) = {
    val params = Utils.parseArgs(args)
    val config = ConfigFactory.load
    val logger = LoggerFactory.getLogger(this.getClass)

    val portions = if (params.learningCurve) (1 to 20).map(_.toDouble / 20) else Seq(1.0)

    val nonFeatures = Seq("--analysis", "--test", "--noTraining", "--learningCurve")
    // This model and results are specified by all input args that represent featuresets
    val fileExt = args.filterNot(nonFeatures.contains).sorted.mkString("").replace("-", "")

    val outputDir = config.getString("classifier") + "/gender/results/" + fileExt
    if (!Files.exists(Paths.get(outputDir))) {
      if (new File(outputDir).mkdir()) logger.info(s"Created output directory $outputDir")
      else logger.info(s"ERROR: failed to create output directory $outputDir")
    }

    val toTrainOn = if (params.runOnTest) {
      logger.info("Loading training accounts...")
      val trainData = FileUtils.loadTwitterAccounts(config.getString("classifiers.gender.trainingData")).toSeq
      logger.info("Loading dev accounts...")
      val devData = FileUtils.loadTwitterAccounts(config.getString("classifiers.gender.devData")).toSeq
      trainData ++ devData
    } else {
      logger.info("Loading training accounts...")
      FileUtils.loadTwitterAccounts(config.getString("classifiers.gender.trainingData")).toSeq
    }

    val followers = if (params.useFollowers) Option(ClassifierImpl.loadFollowers(toTrainOn.map(_._1))) else None
    val followees = if (params.useFollowees) Option(ClassifierImpl.loadFollowees(toTrainOn.map(_._1), "gender")) else None

    val modelDir = s"${config.getString("gender")}/model"
    if (!Files.exists(Paths.get(modelDir))) {
      if (new File(modelDir).mkdir()) logger.info(s"Created output directory $modelDir")
      else logger.error(s"ERROR: failed to create output directory $modelDir")
    }
    val modelFile = s"${config.getString("gender")}/model/$fileExt.dat"

    val classifiers = for {
      portion <- portions
      maxIndex = (portion * toTrainOn.length).toInt
    } yield {
      val (trainAccounts, trainLabels) = toTrainOn.slice(0, maxIndex).unzip

      val gc = new GenderClassifier(
        useUnigrams = params.useUnigrams,
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
        useRT = params.useRT,
        datumScaling = params.datumScaling,
        featureScaling = params.featureScaling
      )

      logger.info("Training classifier...")
      gc.setClassifier(new L1LinearSVMClassifier[String, String]())
      gc.train(trainAccounts, trainLabels, followers, followees)
      // Only save models using full training
      if (maxIndex == toTrainOn.length) gc.subClassifier.get.saveTo(modelFile)

      (portion, maxIndex, gc)
    }
    val toTestOn = if (params.runOnTest) {
      logger.info("Loading test accounts...")
      FileUtils.loadTwitterAccounts(config.getString("classifiers.gender.testData"))
    } else {
      logger.info("Loading dev accounts...")
      FileUtils.loadTwitterAccounts(config.getString("classifiers.gender.devData"))
    }

    val evals = for ((portion, numAccounts, gc) <- classifiers) yield {

      // Set progress bar
      val pb = new me.tongfei.progressbar.ProgressBar("main()", 100)
      pb.start()
      pb.maxHint(toTestOn.size)
      pb.setExtraMessage("Testing on dev accounts...")

      // Classify accounts
      val testSetLabels = toTestOn.values.toSeq
      val predictedLabels = toTestOn.keys.toSeq.map { u =>
        pb.step()
        gc.classify(u)
      }

      pb.stop()

      // Print results
      val (evalMeasures, microAvg, macroAvg) = Eval.evaluate(testSetLabels, predictedLabels, toTestOn.keys.toSeq)

      val evalMetric = evalMeasures(gc.labels.toSeq.sorted.head)
      val precision = evalMetric.P
      val recall = evalMetric.R

      if (portion == 1.0) {
        if (params.fpnAnalysis & gc.subClassifier.nonEmpty &
          (evalMetric.FNAccounts.nonEmpty || evalMetric.FPAccounts.nonEmpty)) {
          // Perform analysis on false negatives and false positives
          println("False negatives:")
          evalMetric.FNAccounts.foreach(account => print(account.handle + "\t"))
          println("\n====")
          outputAnalysis(outputDir + "/analysisFN.txt", "*** False negatives ***\n\n", evalMetric.FNAccounts, gc, gc.labels)

          println("False positives:")
          evalMetric.FPAccounts.foreach(account => print(account.handle + "\t"))
          println("\n====")
          outputAnalysis(outputDir + "/analysisFP.txt", "*** False positives ***\n\n", evalMetric.FPAccounts, gc, gc.labels)
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