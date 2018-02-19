package org.clulab.twitter4food.featureclassifier

import org.clulab.twitter4food.struct._
import org.clulab.twitter4food.util._
import java.io._
import java.nio.file.{Files, Paths}

import com.typesafe.config.ConfigFactory
import org.slf4j.LoggerFactory

object Gender4Diabetes {

  import ClassifierImpl._

  def main(args: Array[String]) = {
    val params = Utils.parseArgs(args)
    val config = ConfigFactory.load
    val logger = LoggerFactory.getLogger(this.getClass)

    val portion = 1.0

    val nonFeatures = Seq("--analysis", "--test", "--noTraining", "--learningCurve")
    // This model and results are specified by all input args that represent featuresets
    val fileExt = args.filterNot(nonFeatures.contains).sorted.mkString("").replace("-", "")

    val outputDir = config.getString("classifier") + "/gender/results/" + fileExt
    if (!Files.exists(Paths.get(outputDir))) {
      if (new File(outputDir).mkdir()) logger.info(s"Created output directory $outputDir")
      else logger.info(s"ERROR: failed to create output directory $outputDir")
    }

//    val genderPartitionFile = config.getString("classifiers.gender.folds")
//    val genderPartitions = FileUtils.readFromCsv(genderPartitionFile).map { user =>
//      user(1).toLong -> user(0).toInt // id -> partition
//    }.toMap

    logger.info("Loading Twitter accounts")
    val genderAccts = FileUtils.loadTwitterAccounts(config.getString("classifiers.gender.data"))
      .toSeq
      .filter(_._1.tweets.nonEmpty)
//      .filter{ case (acct, lbl) => genderPartitions.contains(acct.id)}

    val modelDir = s"${config.getString("gender")}/model"
    if (!Files.exists(Paths.get(modelDir))) {
      if (new File(modelDir).mkdir()) logger.info(s"Created output directory $modelDir")
      else logger.error(s"ERROR: failed to create output directory $modelDir")
    }
    val modelFile = s"${config.getString("gender")}/model/$fileExt.dat"

    val (trainingAccts, trainingLbls) = genderAccts.unzip

    val diabetesPartitionFile = config.getString("classifiers.diabetes.folds")
    val diabetesPartitions = FileUtils.readFromCsv(diabetesPartitionFile).map { user =>
      user(1).toLong -> user(0).toInt // id -> partition
    }.toMap

    val diabetesAccts = FileUtils.loadTwitterAccounts(config.getString("classifiers.diabetes.data"))
      .toSeq
      .filter(_._1.tweets.nonEmpty)
      .filter{ case (acct, lbl) => diabetesPartitions.contains(acct.id)}

    val goldLbls = FileUtils
      .readFromCsv(config.getString("classifiers.diabetes.goldAgeGenderAnnotations"))
      .map(row => row.head.toLong -> row.last) // id -> gender
      .toMap

    val dbAccts = diabetesAccts.unzip._1
    val dbLbls = dbAccts.map(acct => goldLbls(acct.id))

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
      useRT = params.useRT,
      datumScaling = params.datumScaling,
      featureScaling = params.featureScaling
    )

    logger.info("Training classifier...")

    val labelSet = Map("pos" -> "F", "neg" -> "M")
    val highConfPercent = config.getDouble("classifiers.diabetes.highConfPercent")

    val (predictions, bestFreq, bestPerc, avgWeights, falsePos, falseNeg) =
      gc.binaryCVFS(
        dbAccts,
        dbLbls,
        diabetesPartitions,
        portion,
        None,
        None,
        Utils.svmFactory,
        labelSet,
        percentTopToConsider=highConfPercent,
        measure="macro",
        trainingAccts=trainingAccts,
        trainingLbls=trainingLbls
      )

    // Print results
    val (evalMeasures, microAvg, macroAvg) = Eval.evaluate(predictions)

    val evalMetric = if (evalMeasures.keySet contains "F") {
      evalMeasures("F")
    } else {
      logger.debug(s"Labels are {${evalMeasures.keys.mkString(", ")}}. Evaluating on ${evalMeasures.head._1}")
      evalMeasures.head._2
    }
    val precision = evalMetric.P
    val recall = evalMetric.R

    // Write analysis only on full portion
    if (portion == 1.0) {
      if (params.fpnAnalysis) {
        // Perform analysis on false negatives and false positives
        outputAnalysis(outputDir, avgWeights, falsePos, falseNeg)
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
      val predWriter = new BufferedWriter(new FileWriter(outputDir + "/predicted.txt", false))
      predWriter.write(s"gold\tpred\n")
      predictions.foreach(acct => predWriter.write(s"${acct._1}\t${acct._2}\n"))
      predWriter.close()
    }

    val freq = bestFreq.sum.toDouble / bestFreq.length
    val ig = bestPerc.sum / bestPerc.length

    val (gold, pred) = predictions.unzip
    val baseline = Array.fill[String](gold.length)("F")
    val f1 = fMeasure(precision, recall, 1)

    val (baselineEval, baselineMicroAvg, baselineMacroAvg) = Eval.evaluate(predictions.unzip._1.zip(baseline))
    val baselineP = baselineEval("F").P
    val baselineR = baselineEval("F").R
    val baselineF1 = fMeasure(baselineP, baselineR, 1)

    val sig = BootstrapSignificance.bss(gold, baseline, pred, "F", measure = "macro")

    println(s"\n$fileExt\nportion\tfreq_cutoff\tIG%kept\tp\tr\tf1\tmacro\tmicro\tmacro_p-val")
    println(f"baseline\tNA\tNA\t$baselineP%1.5f\t$baselineR%1.5f\t$baselineF1%1.5f\t$baselineMacroAvg%1.5f\t" +
      f"$baselineMicroAvg%1.5f\tNA")
    println(f"$portion%1.2f\t$freq%1.5f\t$ig%1.5f\t$precision%1.5f\t$recall%1.5f\t$f1%1.5f\t" +
      f"$macroAvg%1.5f\t$microAvg%1.5f\t$sig%1.6f")
  }
}