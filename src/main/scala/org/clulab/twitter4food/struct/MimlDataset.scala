package org.clulab.twitter4food.struct

import java.io.Serializable

import org.clulab.learning.Datum
import org.clulab.struct.{Counter, Lexicon}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Parent class for MIML (multi-instance multi-label) classification datasets
  * @author Dane Bell
  * 11/17/2016
  */
abstract class MimlDataset[L, F](
  val labelLexicon: Lexicon[L],
  val featureLexicon: Lexicon[F],
  var posLabels: ArrayBuffer[Set[Int]],
  var negLabels: ArrayBuffer[Set[Int]]
) extends Serializable {

  //def += (datum:MimlDatum[L, F])

  def numFeatures: Int = featureLexicon.size

  def numLabels: Int = labelLexicon.size

  // Could be negLabels.length
  def size = posLabels.length

  def indices = 0 until size

  //def mkDatum(row: Int): MimlDatum[L, F]

  /** Removes features that appear less than threshold times in this dataset. */
  //def removeFeaturesByFrequency(threshold:Int):MimlDataset[L, F]

  /** Creates a new dataset keeping only the features in the given set */
  //def keepOnly(featuresToKeep:Set[Int]):MimlDataset[L, F]
}

class BMimlDataset[L, F](
  ll:Lexicon[L],
  fl:Lexicon[F],
  pls:ArrayBuffer[Set[Int]],
  nls:ArrayBuffer[Set[Int]],
  val features:ArrayBuffer[Array[Array[Int]]]) extends MimlDataset[L, F](ll, fl, pls, nls) {

  def this() = this(
    new Lexicon[L],
    new Lexicon[F],
    new ArrayBuffer[Set[Int]],
    new ArrayBuffer[Set[Int]],
    new ArrayBuffer[Array[Array[Int]]])

  //def mkDatum(row: Int): BMimlDatum[L, F] = new BMimlDatum[L, F]()

//  override def removeFeaturesByFrequency(threshold:Int):MimlDataset[L, F] = {
//    // compute feature frequencies and keep the ones above threshold
//    val counts = countFeatures(features, threshold)
//
//    keepOnly(counts)
//  }
//
//  def countFeatures(fs:ArrayBuffer[Array[Int]], threshold:Int):Set[Int] = {
//    val counts = new Counter[Int]
//    for(d <- fs) {
//      for(f <- d) {
//        counts.incrementCount(f)
//      }
//    }
//    // logger.debug("Total unique features before filtering: " + counts.size)
//
//    val passed = new mutable.HashSet[Int]()
//    for(f <- counts.keySet) {
//      if(counts.getCount(f) >= threshold)
//        passed += f
//    }
//    // logger.debug(s"Total unique features after filtering with threshold $threshold: ${passed.size}")
//
//    passed.toSet
//  }
//
//  override def keepOnly(featuresToKeep:Set[Int]):MimlDataset[L, F] = {
//    // map old feature ids to new ids, over the filtered set
//    val featureIndexMap = new mutable.HashMap[Int, Int]()
//    var newId = 0
//    for(f <- 0 until featureLexicon.size) {
//      if(featuresToKeep.contains(f)) {
//        featureIndexMap += f -> newId
//        newId += 1
//      }
//    }
//
//    // construct the new dataset with the filtered features
//    val newFeatures = new ArrayBuffer[Array[Int]]
//    for{
//      datum <- features.indices
//      instance <- features(datum).indices
//    } {
//      val nfs = keepOnlyRow(features(datum)(instance), featureIndexMap)
//      newFeatures += nfs
//    }
//
//    new BMimlDataset[L, F](labelLexicon, featureLexicon.mapIndicesTo(featureIndexMap.toMap), labels, newFeatures)
//  }
//
//  def keepOnlyRow(feats:Array[Int], featureIndexMap:mutable.HashMap[Int, Int]):Array[Int] = {
//    val newFeats = new ArrayBuffer[Int]()
//
//    for(i <- feats.indices) {
//      val f = feats(i)
//      if(featureIndexMap.contains(f)) {
//        newFeats += featureIndexMap.get(f).get
//      }
//    }
//
//    newFeats.toArray
//  }
}

class RMimlDataset[L, F](
  ll:Lexicon[L],
  fl:Lexicon[F],
  pls:ArrayBuffer[Set[Int]],
  nls:ArrayBuffer[Set[Int]],
  fs:ArrayBuffer[Array[Array[Int]]],
  val values:ArrayBuffer[Array[Array[Double]]]) extends BMimlDataset[L, F](ll, fl, pls, nls, fs) {

  def this() = this(
    new Lexicon[L],
    new Lexicon[F],
    new ArrayBuffer[Set[Int]],
    new ArrayBuffer[Set[Int]],
    new ArrayBuffer[Array[Array[Int]]],
    new ArrayBuffer[Array[Array[Double]]])

  /**
  def getFeatureCounts: Array[Double] = {
    val counts = new Counter[Int]()
    for {
      i <- data
      j <- i
      k <- j
    } {
      counts.incrementCount(k)
    }
    counts.toSeq.sortBy(_._1).map(_._2).toArray
  }


  def applyFeatureCountThreshold(threshold: Int) {
    val counts: Array[Double] = getFeatureCounts
    val newFeatureIndex: HashIndex[F] = new HashIndex[F]()
    val featMap: Array[Int] = new Array[Int](featureIndex.size)
    for (i <- featMap.indices) {
      val feat: F = featureIndex.get(i)
      if (counts(i) >= threshold) {
        val newIndex: Int = newFeatureIndex.size
        newFeatureIndex.add(feat)
        featMap(i) = newIndex
      }
      else {
        featMap(i) = -1
      }
    }
    featureIndex = newFeatureIndex
    for {
      i <- data.indices
      j <- data(i).indices
    } {
      val featList: ArrayBuffer[Int] = ArrayBuffer[Int](data(j).indices)
      for (k <- data(i)(j).indices) {
        if (featMap(k) >= 0) {
          featList.append(featMap(k))
        }
      }
      data(i)(j) = new Array[Int](featList.size)
      for (k <- data(i)(j).indices) {
        {
          data(i)(j)(k) = featList(k)
        }
      }
    }
  }

  def addDatum(yPos: Set[L], yNeg: Set[L], group: Seq[Datum[L, F]]) {
    val features = new ArrayBuffer[Seq[F]]()
    for (datum <- group) {
      features.append(datum.asFeatures)
    }
    add(yPos, yNeg, features)
  }

  def add(yPos: Set[L], yNeg: Set[L], group: ArrayBuffer[Seq[F]]) {
    ensureSize
    addPosLabels(yPos)
    addNegLabels(yNeg)
    addFeatures(group)
    size += 1
  }

  protected def addFeatures(group: ArrayBuffer[Seq[F]]) {
    val groupFeatures: Array[Array[Int]] = new Array[Array[Int]](group.size)
    var datumIndex: Int = 0
    for (features <- group) {
      val intFeatures: Array[Int] = new Array[Int](features.size)
      var j: Int = 0
      for (feature <- features) {
        featureIndex.add(feature)
        val index: Int = featureIndex.indexOf(feature)
        if (index >= 0) {
          intFeatures(j) = featureIndex.indexOf(feature)
          j += 1
        }
      }
      val trimmedFeatures: Array[Int] = new Array[Int](j)
      System.arraycopy(intFeatures, 0, trimmedFeatures, 0, j)
      groupFeatures(datumIndex) = trimmedFeatures
      datumIndex += 1
    }
    assert((datumIndex == group.size))
    data(size) = groupFeatures
  }

  protected def addPosLabels(labels: Set[L]) {
    labelIndex.addAll(labels)
    var newLabels = Set[Int]()
    for (l <- labels) {
      newLabels += labelIndex.indexOf(l)
    }
    posLabels = posLabels append newLabels
  }

  protected def addNegLabels(labels: Set[L]) {
    labelIndex.addAll(labels)
    val newLabels: Nothing = new Nothing
    for (l <- labels) {
      newLabels.add(labelIndex.indexOf(l))
    }
    negLabels(size) = newLabels
  }

  protected def ensureSize {
    if (posLabels.length == size) {
      var newLabels: Array[L] = new Array[L](size * 2)
      System.arraycopy(posLabels, 0, newLabels, 0, size)
      posLabels = newLabels
      newLabels = new Array[L](size * 2)
      System.arraycopy(negLabels, 0, newLabels, 0, size)
      negLabels = newLabels
      val newData: Array[Array[Array[Int]]] = new Array[Array[Array[Int]]](size * 2)
      System.arraycopy(data, 0, newData, 0, size)
      data = newData
    }
  }
    */
}
