package org.clulab.twitter4food.util

import java.io.File

import com.typesafe.config.ConfigFactory
import org.slf4j.LoggerFactory

import scala.util.Random

/**
  * Test for one-tailed statistical significance of all systems compared to a named baseline
  */
object BootstrapSignificance {

  def microF1(gold: Seq[String], pred: Seq[String]): Double = {
    val tp: Double = (for (i <- gold.indices) yield if (gold(i) == pred(i)) 1 else 0).sum
    val fn: Double = (for {
      lbl <- gold.distinct
      i <- gold.indices
      if gold(i) == lbl && pred(i) != lbl
    } yield 1).sum
    val fp: Double = (for {
      lbl <- gold.distinct
      i <- gold.indices
      if gold(i) != lbl && pred(i) == lbl
    } yield 1).sum

    2.0 * tp / (2 * tp + fn + fp)
  }

  def main(args: Seq[String]) = {
    val logger = LoggerFactory.getLogger("BootstrapSignificance")
    val config = ConfigFactory.load

    val baselineFeatures = config.getString("classifiers.overweight.baseline")
    val predictionDir = new File(config.getString("classifiers.overweight.results"))
    val reps = if (args.nonEmpty) args.head.toInt else 10000
    logger.info(s"repetitions: $reps")

    // The directory of results files must exist
    assert(predictionDir.exists && predictionDir.isDirectory)
    val folders = predictionDir.listFiles.filter(_.isDirectory)

    // gather each set of (gold data, prediction) from each result available
    // IndexedSeq for very frequent indexing later
    val predictionsWithGold: Map[String, IndexedSeq[(String, String)]] = (for {
        folder <- folders.toSeq
        if folder.list.contains("predictions.txt")
        predFile = scala.io.Source.fromFile(folder.getPath + "/predictions.txt")
        preds = predFile.getLines.map(_.stripLineEnd.split("\t")).map(line => (line(0), line(1))).toIndexedSeq
      } yield folder.getName -> preds).toMap

    // If the baseline is not present, we can't compare against it.
    assert(predictionsWithGold.keys.toSeq.contains(baselineFeatures))

    val (gold, baseline) = predictionsWithGold(baselineFeatures).unzip

    // Ignore results that have different Gold annotations and thus different users or user order
    val comparable = predictionsWithGold.filter(pred => pred._2.unzip._1 == gold)
    val incomparable = predictionsWithGold.keySet diff comparable.keySet
    if(incomparable.nonEmpty) {
      logger.info(s"""$incomparable did not have the same gold annotations as baseline""")
    }

    // Don't bother performing calculation of baseline against itself
    val predictions = comparable
      .filterKeys(_ != baselineFeatures)
      .map(featureSet => featureSet._1 -> featureSet._2.unzip._2)

    val betterThanBaseline: Map[String, scala.collection.mutable.ListBuffer[Double]] = (for {
      key <- comparable.keys
    } yield key -> new scala.collection.mutable.ListBuffer[Double]).toMap

    // TODO: Can we make this more efficient? Indexing is on IndexedSeq, so constant time per index
    for {
      i <- 0 until reps
      sampleIdx = for (j <- gold.indices) yield Random.nextInt(gold.length - 1) // random sample with replacement
      sampleGold = for (j <- sampleIdx) yield gold(j)
      featureSet <- predictions.keys  // same sample applied to each eligible featureSet
      pred = predictions(featureSet)
      samplePred = for (j <- sampleIdx) yield pred(j) // predictions for sampled accts
      sampleBase = for (j <- sampleIdx) yield baseline(j) // baseline predictions for sampled accts
    } {
      val baselineF1 = microF1(sampleGold, sampleBase)
      val predF1 = microF1(sampleGold, samplePred)
      betterThanBaseline(featureSet)(i) = if (predF1 > baselineF1) 1.0 else 0.0
    }

  }
}