package org.clulab.twitter4food.util

import java.io.File

import com.typesafe.config.ConfigFactory
import org.slf4j.LoggerFactory

import scala.util.Random

/**
  * Test for one-tailed statistical significance of all systems compared to a named baseline
  * @author Dane Bell
  */
object BootstrapSignificance {

  def bss(gold: Seq[String],
          baseline: Seq[String],
          predicted: Seq[String],
          label: String,
          reps: Int = 10000,
          measure: String = "f1"
         ): Double = {

    def metric(m: String, lbl: String, gold: Seq[String], preds: Seq[String]): Double = {
      m match {
        case "macro" => Eval.macroOnly(gold, preds)
        case "micro" => Eval.microOnly(gold, preds)
        case other => Eval.f1ForLabel(lbl)(gold zip preds)
      }
    }

    val betterThanBaseline = Array.fill[Double](reps)(0)

    val pb = new me.tongfei.progressbar.ProgressBar("bootstrap", 100)
    pb.start()
    pb.maxHint(reps)
    pb.setExtraMessage("sampling...")

    // for each rep, randomly sample indices once, then compare the baseline's F1 to each other model's
    for {
      i <- (0 until reps).par
      sampleIdx = for (j <- gold.indices) yield Random.nextInt(gold.length - 1) // random sample with replacement
      sampleGold = sampleIdx.map(gold.apply)
      samplePred = sampleIdx.map(predicted.apply)
      sampleBase = sampleIdx.map(baseline.apply)
    } {
      val predScore = metric(measure, label, sampleGold, samplePred)
      val baselineScore = metric(measure, label, sampleGold, sampleBase)
      if (predScore > baselineScore) betterThanBaseline(i) = 1
      pb.step()
    }
    pb.stop()

    1.0 - (betterThanBaseline.sum / reps.toDouble)
  }

  case class Config(
    variable: String = "overweight",
    scoreMetric: String = "Overweight",
    repetitions:Int = 10000)

  def main(args: Array[String]): Unit = {
    def parseArgs(args: Array[String]): Config = {
      val parser = new scopt.OptionParser[Config]("bootstrapping") {
        head("bootstrapping", "0.x")
        opt[String]('v', "variable") action { (x, c) =>
          c.copy(variable = x)
        } text "classifier to evaluate"
        opt[String]('s', "scoreMetric") action { (x, c) =>
          c.copy(scoreMetric = x)
        } text "metric for scoring; can be 'micro', 'macro', or a variable name for unaveraged F1"
        opt[Int]('r', "repetitions") action { (x, c) =>
          c.copy(repetitions = x)
        } text "number of repetitions in bootstrap"
      }

      parser.parse(args, Config()).get
    }

    val logger = LoggerFactory.getLogger(this.getClass)
    val config = ConfigFactory.load
    val params = parseArgs(args)

    val baselineFeatures = config.getString(s"classifiers.${params.variable}.baseline")
    val predictionDir = new File(config.getString(s"classifiers.${params.variable}.results"))

    // Prefer overweightF1 >> microF1 >> macroF1
    def scoreMetric(gold: Seq[String], pred: Seq[String]): Double = {
      params.scoreMetric match {
        case "macro" => Eval.macroOnly(gold.zip(pred))
        case "micro" => Eval.microOnly(gold.zip(pred))
        case lbl => Eval.f1ForLabel(lbl)(gold.zip(pred))
      }
    }

    // The directory of results files must exist
    assert(predictionDir.exists && predictionDir.isDirectory)
    val folders = predictionDir.listFiles.filter(_.isDirectory)

    // gather each set of (gold data, prediction) from each result available
    // IndexedSeq for very frequent indexing later
    val predictionsWithGold: Map[String, IndexedSeq[(String, String)]] = (for {
        folder <- folders.toSeq
        if folder.list.contains("predicted.txt")
        predFile = scala.io.Source.fromFile(folder.getPath + "/predicted.txt")
        preds = predFile
          .getLines
          .map(_.stripLineEnd.split("\t"))
          .map(line => (line(0), line(1)))
          .toIndexedSeq
          .tail // first row is header info
          .sortBy(_._1) // gold columns are in different orders, so sort
      } yield folder.getName -> preds).toMap

    // If the baseline is not present, we can't compare against it.
    assert(predictionsWithGold.keys.toSeq.contains(baselineFeatures))

    val (gold, baseline) = predictionsWithGold(baselineFeatures).unzip

    // Ignore results that have different Gold annotations and thus different users or user order
    val comparable = predictionsWithGold.filter(pred => pred._2.unzip._1 == gold)
    val incomparable = predictionsWithGold.keySet diff comparable.keySet
    if(incomparable.nonEmpty) {
      logger.debug(s"""$incomparable did not have the same gold annotations as baseline""")
    }

    val predictions = comparable.map(featureSet => featureSet._1 -> featureSet._2.unzip._2)

    // initialize a buffer for tracking whether each model's F1 exceeds the baseline
    val betterThanBaseline: Map[String, scala.collection.mutable.ListBuffer[Double]] = (for {
      key <- predictions.keys
    } yield key -> new scala.collection.mutable.ListBuffer[Double]).toMap

    logger.info(s"classifier: ${params.variable}, repetitions: ${params.repetitions}, models: ${predictions.size}")

    val pb = new me.tongfei.progressbar.ProgressBar("bootstrap", 100)
    pb.start()
    pb.maxHint(params.repetitions * betterThanBaseline.size)
    pb.setExtraMessage("sampling...")

    // for each rep, randomly sample indices once, then compare the baseline's F1 to each other model's
    for {
      i <- 0 until params.repetitions
      sampleIdx = for (j <- gold.indices) yield Random.nextInt(gold.length - 1) // random sample with replacement
      sampleGold = for (j <- sampleIdx) yield gold(j) // ground truth labels for sampled accts
      featureSet <- predictions.keys  // same sample applied to each eligible featureSet
      pred = predictions(featureSet)
      samplePred = for (j <- sampleIdx) yield pred(j) // comparison predictions for sampled accts
      sampleBase = for (j <- sampleIdx) yield baseline(j) // baseline predictions for sampled accts
    } {
      val baselineF1 = scoreMetric(sampleGold, sampleBase)
      val predF1 = scoreMetric(sampleGold, samplePred)
      betterThanBaseline(featureSet).append(if (predF1 > baselineF1) 1.0 else 0.0)
      pb.step()
    }

    pb.stop()

    // Calculate all stats just to be sure.
    val stats = (for (k <- predictions.keys) yield {
      val (eval, macroAvg, microAvg) = Eval.evaluate(gold, predictions(k))
      val lbl = if (!Seq("micro","macro").contains(params.scoreMetric)) params.scoreMetric else gold.distinct.sorted.head
      k -> (eval(lbl).P, eval(lbl).R, eval(lbl).F, microAvg, macroAvg)
    }).toMap

    // print out results
    println("model\tprecision\trecall\toverweight F1\tmicro F1\tmacro F1\tpval")
    betterThanBaseline.toSeq.sortBy(_._1).reverse.foreach{
      case (featureSet, isBetter) =>
        val baselineLabel = if (featureSet == baselineFeatures) " (baseline)" else ""
        println(f"$featureSet$baselineLabel\t${stats(featureSet)._1}%1.4f\t${stats(featureSet)._2}%1.4f\t" +
          f"${stats(featureSet)._3}%1.4f\t${stats(featureSet)._4}%1.4f\t${stats(featureSet)._5}%1.4f\t" +
          f"${1.0 - isBetter.sum / params.repetitions.toDouble}%1.4f")
    }
  }
}