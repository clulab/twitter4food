package org.clulab.twitter4food.miml

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
  def main(args:Array[String]): Unit = {
    val logger = LoggerFactory.getLogger(this.getClass)

    val params = Utils.parseArgs(args)
    val config = ConfigFactory.load

    val unrel = RelationMention.UNRELATED

    // load all accounts labeled either "Overweight" or "Not overweight"
    logger.info("Loading Twitter accounts")
    val owAccts = FileUtils.load(config.getString("classifiers.miml.pos"))
      .toSeq
      .filter(_._1.tweets.nonEmpty)

    // load human/organization dataset, discard humans, and mark organizations as nil ("_NF")
    val orgAccts = FileUtils.load(config.getString("classifiers.miml.neg"))
      .toSeq
      .filter(_._2 == "org")
      .filter(_._1.tweets.nonEmpty)
      .map{ case (acct, lbl) => (acct, unrel) }

    // Scale number of accounts so that weights aren't too biased against Overweight
    val desiredProps = Map( "Overweight" -> 0.4, "Not overweight" -> 0.4, unrel -> 0.2 )
    val subsampled = Utils.subsample(owAccts ++ orgAccts, desiredProps)

    val dataset = OverweightDataConstructor.constructMimlDataset(subsampled, params)

    val numFolds = config.getInt("classifiers.miml.folds")
    val seed = 71
    val partitions = mkStratifiedTrainTestFolds[String, String](numFolds, dataset, seed)

    for (partition <- partitions) {
      val extractor = new HoffmannExtractor(config.getInt("classifiers.miml.epochs"))
      val (train, test) = cutDataset(dataset, partition)
      dataset.randomize(1)
      dataset.applyFeatureCountThreshold(config.getDouble("classifiers.miml.featureCountThreshold"))
      extractor.train(train)
//      var relations = new java.util.ArrayList[java.util.List[java.util.Collection[String]]]()
//      var goldLabels = new java.util.ArrayList[java.util.Set[String]]()
//      var predictedLabels = new java.util.ArrayList[java.util.Set[String]]()
//      val score = extractor.test(relations, goldLabels, predictedLabels)
    }
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
      .groupBy(idx => dataset.getLabelsArray()(idx).toArray.asInstanceOf[Array[Int]].head)
    val folds = (for (i <- 0 until numFolds) yield (i, new ArrayBuffer[TrainTestFold])).toMap

    for {
      c <- 0 until dataset.labelIndex().size()
      i <- 0 until numFolds
    } {
      val cds = byClass(c)
      val classSize = cds.length
      val foldSize = classSize / numFolds
      val startTest = i * foldSize
      val endTest = if (i == numFolds - 1) math.max(classSize, (i + 1) * foldSize) else (i + 1) * foldSize

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
    val featureIndex = dataset.featureIndex

    val dataJava = new java.util.ArrayList[java.util.ArrayList[java.util.ArrayList[F]]]()
    dataset.getDataArray.foreach { row =>
      val rowJava = new java.util.ArrayList[java.util.ArrayList[F]]()
      row.foreach { inst =>
        val instJava = new java.util.ArrayList[F]()
        inst.foreach { ix =>
          instJava.add(featureIndex.get(ix))
        }
        rowJava.add(instJava)
      }
      dataJava.add(rowJava)
    }

    val valueJava = new java.util.ArrayList[java.util.ArrayList[java.util.ArrayList[Double]]]()
    dataset.getValueArray.foreach { row =>
      val rowJava = new java.util.ArrayList[java.util.ArrayList[Double]]()
      row.foreach { inst =>
        val instJava = new java.util.ArrayList[Double]()
        inst.foreach { v =>
          instJava.add(v)
        }
        rowJava.add(instJava)
      }
      valueJava.add(rowJava)
    }

    val labelIndex = dataset.labelIndex
    val labels = dataset.getLabelsArray.map(lbls => labelIndex.get(lbls.iterator().next())) // assume one label

    val train = new RvfMLDataset[L, F](fold.train.length)
    fold.train.foreach{ i =>
      val labelSet = new java.util.HashSet[L](1)
      labelSet.add(labels(i))
      train.add(labelSet,
        dataJava.get(i).asInstanceOf[java.util.List[java.util.List[F]]],
        valueJava.get(i).asInstanceOf[java.util.List[java.util.List[java.lang.Double]]])
    }

    val test = new RvfMLDataset[L, F](fold.test.length)
    fold.test.foreach{ i =>
      val labelSet = new java.util.HashSet[L](1)
      labelSet.add(labels(i))
      train.add(labelSet,
        dataJava.get(i).asInstanceOf[java.util.List[java.util.List[F]]],
        valueJava.get(i).asInstanceOf[java.util.List[java.util.List[java.lang.Double]]])
    }
    (train,test)
  }
}