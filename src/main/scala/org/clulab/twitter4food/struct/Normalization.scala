package org.clulab.twitter4food.struct

import org.clulab.learning.{Datasets, RVFDataset, RVFDatum}
import org.clulab.struct.Counter

/**
  * @author danebell
  * @date 06-10-2016
  */

object Normalization {
  /** The actual scaling formula taken from svm-scale
    * Copied from edu.arizona.sista.learning.Datasets
    * */
  private def scale(value:Double, min:Double, max:Double, lower:Double, upper:Double):Double = {
    if(min == max) return upper

    // the result will be a value in [lower, upper]
    lower + (upper - lower) * (value - min) / (max - min)
  }

  /**
    * Scale each [[RVFDatum]] in an [[RVFDataset]] in place according to its own minimum and maximum feature values
    *
    * @param dataset [[RVFDataset]] to be normalized (in place)
    * @param lower lower bound of normalized values (e.g. -1 or 0), inclusive
    * @param upper upper bound of normalized values (e.g. 1), inclusive
    * @tparam L label type
    * @tparam F feature type
    */
  def scaleByDatum[L,F](dataset:RVFDataset[L,F], lower:Double, upper:Double): Unit = {
    // scan the dataset once and keep track of min/max for each feature
    for(i <- dataset.indices) {
      val datumMin = dataset.values(i).min
      val datumMax = dataset.values(i).max
      for(j <- dataset.features(i).indices) {
        dataset.values(i)(j) = scale(dataset.values(i)(j), datumMin, datumMax, lower, upper)
      }
    }
  }

  /**
    * Scale each value of a [[Counter]] in place according to the Counter's minimum and maximum values
    *
    * @param counter [[Counter]] to be normalized (in place)
    * @param lower lower bound of normalized values (e.g. -1 or 0), inclusive
    * @param upper upper bound of normalized values (e.g. 1), inclusive
    * @tparam T Type of the [[Counter]] keys, e.g. [[String]]
    */
  def scaleByDatum[T](counter:Counter[T], lower:Double, upper:Double): Unit = {
    val minimum = counter.argMin._2
    val maximum = counter.argMax._2
    counter.map {
      case (k, v) => scale(v, minimum, maximum, lower, upper)
    }
  }


  /**
    * Scale each feature in an [[RVFDataset]] in place according to the minimum and maximum values of the feature
    *
    * @param dataset dataset to be normalized (in place)
    * @param lower lower bound of normalized values (e.g. -1 or 0), inclusive
    * @param upper upper bound of normalized values (e.g. 1), inclusive
    * @tparam L label type
    * @tparam F feature type
    */
  def scaleByFeature[L,F](dataset:RVFDataset[L,F], lower:Double, upper:Double): Unit = {
    Datasets.svmScaleDataset(dataset, lower, upper)
  }

  /**
    * Scale a [[Counter]] in place to have the same total size as a reference [[Counter]]
    *
    * @param toScale a [[Counter]] that we want to normalize to another [[Counter]]'s size.
    * @param scaleBy the reference [[Counter]]
    * @tparam T the type of the [[Counter]]s, e.g. [[String]]
    */
  def scaleByCounter[T](toScale:Counter[T], scaleBy:Counter[T]): Unit = {
    val scaled = toScale * (scaleBy.getTotal / toScale.getTotal)
    toScale.keySet.foreach(k => toScale.setCount(k, scaled.getCount(k)))
  }

  /**
    * Scale a [[Counter]] so that the sum of all the values in it will equal 1
    *
    * @param counter [[Counter]] to be scaled (in place)
    * @tparam T Type of the [[Counter]] keys, e.g. [[String]]
    */
  def scaleToUnitLength[T](counter:Counter[T]): Unit = {
    counter.keySet.foreach(k => counter.setCount(k, counter.proportion(k)))
  }
}

