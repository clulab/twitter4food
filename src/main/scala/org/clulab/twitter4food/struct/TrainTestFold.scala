package org.clulab.twitter4food.struct

import org.clulab.learning.Dataset

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/** Used by Stratified K-fold CV */
case class TrainTestFold(test: Seq[Int], train: Seq[Int]) {
  def merge(other: TrainTestFold): TrainTestFold = {
    new TrainTestFold(this.test ++ other.test, this.train ++ other.train)
  }
}

/** Used by Stratified K-fold CV */
case class TrainDevTestFold(test: Seq[Int], dev: Seq[Int], train: Seq[Int]) {
  def merge(other: TrainDevTestFold): TrainDevTestFold = {
    new TrainDevTestFold(this.test ++ other.test, this.dev ++ other.dev, this.train ++ other.train)
  }
}

object TrainTestFold {
  /**
    * Returns a [[Seq]] of [[TrainTestFold]]s given unique ids and a map of what partition they belong in.
    */
  def foldsFromIds(ids: Seq[Long], partitions: Map[Long, Int], portion: Double = 1.0): Seq[TrainTestFold] = {
    // make our map from ID to partition into a map from partition to sequence of IDs
    val partToId = partitions.toSeq.groupBy(_._2).map{ case (grp, ids) => grp -> Random.shuffle(ids.unzip._1) }

    // reduce the size of each partition (at random) to portion size
    val trainPart = for {
      (grp, ids) <- partToId
      sampled = ids.take((portion * ids.length).round.toInt)
      s <- sampled
    } yield s -> grp

    // test folds will contain all indices so that evaluation is always the same
    val teix = ids.zipWithIndex.filter{ case (id, ix) => partitions.contains(id) }
    val idxToTest = for ((id, idx) <- teix) yield {
      idx -> partitions(id)
    }
    val testFolds = idxToTest.groupBy(_._2).map{ case (p, is) => p -> is.map(_._1).toSet }

    // train folds will contain only a portion of their originals
    val trix = ids.zipWithIndex.filter{ case (id, ix) => trainPart.contains(id) }
    val idxToTrain = for ((id, idx) <- trix) yield {
      idx -> partitions(id)
    }
    val trainIndices = ids.indices.filter(i => trainPart.keys.toSeq.contains(ids(i))).toSet
    val trainFolds = idxToTrain.groupBy(_._2).map{ case (p, is) => p -> is.map(_._1).toSet }

    val numPartitions = partitions.values.max + 1 // 0 indexed
    for (p <- 0 until numPartitions) yield {
      TrainTestFold(testFolds(p).toSeq, (trainIndices -- trainFolds(p)).toSeq)
    }
  }

  /** Creates dataset folds to be used for cross validation */
  def mkStratifiedTrainTestFolds[L, F](
                                        numFolds:Int,
                                        dataset:Dataset[L, F],
                                        seed:Int
                                      ): Iterable[TrainTestFold] = {
    val r = new Random(seed)

    val byClass: Map[Int, Seq[Int]] = r.shuffle[Int, IndexedSeq](dataset.indices).groupBy(idx => dataset.labels(idx))
    val folds = (for (i <- 0 until numFolds) yield (i, new ArrayBuffer[TrainTestFold])).toMap

    for {
      c <- 0 until dataset.numLabels
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
}

object TrainDevTestFold {
  /**
    * Returns a [[Seq]] of [[TrainDevTestFold]]s given unique ids and a map of what partition they belong in.
    * The test fold is the same for all [[TrainDevTestFold]]s to maintain the independence of test throughout feature
    * selection.
    */
  def devFoldsFromIds(ids: Seq[Long], partitions: Map[Long, Int]): Seq[TrainDevTestFold] = {
    val lastPartition = partitions.values.max
    val allIndices = ids.indices.toSet
    val idxToFold = for ((id, idx) <- ids.zipWithIndex) yield {
      idx -> partitions(id)
    }
    val foldToIndices = idxToFold.groupBy(_._2).map{ case (p, is) => p -> is.map(_._1).toSet }
    val test = foldToIndices(lastPartition) // the test partition will be the same in all cases
    for (p <- 0 until lastPartition) yield {
      TrainDevTestFold(test.toSeq, foldToIndices(p).toSeq, (allIndices -- foldToIndices(p) -- test).toSeq)
    }
  }

  /** Creates dataset folds to be used for cross validation */
  def mkStratifiedTrainDevTestFolds[L, F](
                                           numFolds:Int,
                                           dataset:Dataset[L, F],
                                           seed:Int
                                         ): Iterable[TrainDevTestFold] = {
    val r = new Random(seed)

    val byClass: Map[Int, Seq[Int]] = r.shuffle[Int, IndexedSeq](dataset.indices).groupBy(idx => dataset.labels(idx))
    val folds = (for (i <- 0 until numFolds) yield (i, new ArrayBuffer[TrainDevTestFold])).toMap

    for {
      c <- 0 until dataset.numLabels
      i <- 0 until numFolds
      j = (i + 1) % numFolds
    } {
      val cds = byClass(c)
      val classSize = cds.length
      val foldSize = classSize / numFolds
      val startTest = i * foldSize
      val endTest = if (i == numFolds - 1) math.max(classSize, (i + 1) * foldSize) else (i + 1) * foldSize
      val startDev = j * foldSize
      val endDev = if (j == numFolds - 1) math.max(classSize, (j + 1) * foldSize) else (j + 1) * foldSize

      val nonTrain = (startTest until endTest) ++ (startDev until endDev)
      val trainFolds = cds.indices.filterNot(ix => nonTrain.contains(ix))

      folds(i) += new TrainDevTestFold(cds.slice(startTest, endTest), cds.slice(startDev, endDev), trainFolds.map(cds.apply))
    }
    folds.map{ dsfSet => dsfSet._2.reduce(_ merge _) }
  }
}

