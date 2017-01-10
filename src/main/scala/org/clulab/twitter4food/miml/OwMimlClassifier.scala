package org.clulab.twitter4food.miml

import java.util

import com.typesafe.config.ConfigFactory
import org.clulab.twitter4food.util.{FileUtils, Utils}
import org.slf4j.LoggerFactory
import edu.stanford.nlp.ie.machinereading.structure.RelationMention
import org.clulab.twitter4food.featureclassifier.TrainTestFold
import org.clulab.twitter4food.struct.RvfMLDataset

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scalaj.collection.Imports._

object OwMimlClassifier {

  val logger = LoggerFactory.getLogger(this.getClass)

  def main(args:Array[String]): Unit = {
    val logger = LoggerFactory.getLogger(this.getClass)

    val params = Utils.parseArgs(args)
    val config = ConfigFactory.load

//    val unrelated = RelationMention.UNRELATED

    // load all accounts labeled either "Overweight" or "Not overweight"
    logger.info("Loading Twitter accounts")
    val owAccts = FileUtils.load(config.getString("classifiers.miml.pos"))
      .toSeq
      .filter(_._1.tweets.nonEmpty)

    // load human/organization dataset, discard humans, and mark organizations as nil ("_NF")
//    val orgAccts = FileUtils.load(config.getString("classifiers.miml.neg"))
//      .toSeq
//      .filter(_._2 == "org")
//      .filter(_._1.tweets.nonEmpty)
//      .map{ case (acct, lbl) => (acct, unrelated) }

    // Scale number of accounts so that weights aren't too biased against Overweight
//    val desiredProps = Map( "Overweight" -> 0.4, "Not overweight" -> 0.4, unrelated -> 0.2 )
    val desiredProps = Map( "Overweight" -> 0.5, "Not overweight" -> 0.5)
//    val subsampled = Utils.subsample(owAccts ++ orgAccts, desiredProps)
    val subsampled = Utils.subsample(owAccts, desiredProps)

    val dataset = OverweightDataConstructor.constructMimlDataset(subsampled, params)

    val numFolds = config.getInt("classifiers.miml.folds")
    val seed = 71
    val partitions = mkStratifiedTrainTestFolds[String, String](numFolds, dataset, seed)

    val scores = for (partition <- partitions) yield {
      val extractor = new HoffmannExtractor(config.getInt("classifiers.miml.epochs"))
      logger.info("Randomizing partition...")
      dataset.randomize(1)
      //logger.info("Applying feature count threshold...")
      //dataset.applyFeatureCountThreshold(config.getDouble("classifiers.miml.featureCountThreshold"))
      logger.info("Preparing partition...")
      val (train, test) = cutDataset(dataset, partition)
      logger.info("Training...")
      extractor.train(train)

      val wts = extractor.zWeights

      wts.zipWithIndex.foreach { case(lblWeights, ix) =>
        val topWeights = lblWeights.avgWeights.zipWithIndex.sortBy{ case (value, i) => value }.takeRight(10).reverse
        val printWeights = topWeights.map{ case (value, i) => s"${train.featureIndex.get(ix)}: $value"}
        logger.debug(s"${train.labelIndex.get(ix)}: ${printWeights.mkString(", ")}")
      }

      val pred = extractor.classifyAccounts(test)
      logger.debug(s"${pred.size()} predictions...")

      // val score = HoffmannExtractor.score(test.getLabelsArray, pred, test.labelIndex.indexOf("Overweight"))
      val score = HoffmannExtractor.score(test.getLabelsArray, pred)
      val p = score.first.toDouble
      val r = score.second.toDouble
      val f = score.third.toDouble
      (p, r, f)
    }

    val ps = scores.map(_._1).toArray
    val rs = scores.map(_._2).toArray
    val fs = scores.map(_._3).toArray

    val p = ps.sum / ps.length
    val r = rs.sum / rs.length
    val f = fs.sum / fs.length

    val modelName = args.sorted.mkString("").replaceAll("-", "")

    displayScores(modelName, p, r, f)
  }

  /** Creates dataset folds to be used for cross validation */
  def mkStratifiedTrainTestFolds[L, F](
    numFolds:Int,
    dataset:RvfMLDataset[L, F],
    seed:Int
  ): Iterable[TrainTestFold] = {
    val r = new Random(seed)

    val byClass: Map[Int, Seq[Int]] = r
      .shuffle[Int, IndexedSeq](dataset.getDataArray.indices)
      .groupBy(idx => dataset.getLabelsArray()(idx).asScala.head)
    val folds = (for (i <- 0 until numFolds) yield (i, new ArrayBuffer[TrainTestFold])).toMap

    for {
      c <- 0 until dataset.labelIndex().size()
      i <- 0 until numFolds
    } {
      val cds = byClass(c)
      val classSize = cds.length
      val foldSize = classSize / numFolds
      val startTest = i * foldSize
      val endTest = if (i == numFolds - 1) classSize else (i + 1) * foldSize

      val trainFolds = new ArrayBuffer[Int]
      if(startTest > 0)
        trainFolds ++= cds.slice(0, startTest)
      if(endTest < classSize)
        trainFolds ++= cds.slice(endTest, classSize)

      folds(i) += new TrainTestFold(cds.slice(startTest, endTest), trainFolds)
    }
    folds.map{ dsfSet => dsfSet._2.reduce(_ merge _) }
  }

  // Very inefficient creation of new datasets.
  // TODO: pass indices to HoffmanExtractor.train instead of whole datasets
  def cutDataset[L, F](dataset:RvfMLDataset[L, F], fold:TrainTestFold): (RvfMLDataset[L,F], RvfMLDataset[L,F]) = {
    val labelIndex = dataset.labelIndex
    val labels = dataset.getLabelsArray.map(lbls => labelIndex.get(lbls.iterator().next())) // assume one label

    val train = new RvfMLDataset[L, F](fold.train.length)
    fold.train.foreach{ i =>
      val labelSet = new java.util.HashSet[L](1)
      labelSet.add(labels(i))
      train.add(labelSet,
        dataset.getFeaturesAt(i),
        dataset.getValuesAt(i),
        dataset.getTweetsAt(i)
      )
    }

    val featureIndex = train.featureIndex

    val test = new RvfMLDataset[L, F](fold.test.length)
    test.featureIndex = featureIndex
    fold.test.foreach{ i =>
      val labelSet = new java.util.HashSet[L](1)
      labelSet.add(labels(i))
      test.add(labelSet,
        dataset.getFeaturesAt(i),
        dataset.getValuesAt(i),
        dataset.getTweetsAt(i)
      )
    }
    (train,test)
  }

  def displayScores(features: String, p: Double, r: Double, f: Double): Unit = {
    val boundary = "-" * 50
    println(boundary)
    println("feats\tprec\trecl\tf1")
    println(boundary)
    println(f"$features\t$p%1.4f\t$r%1.4f\t$f%1.4f\t")
    println(boundary)
  }
}