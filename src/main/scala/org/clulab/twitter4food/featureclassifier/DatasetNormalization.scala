package org.clulab.twitter4food.featureclassifier

import edu.arizona.sista.learning.{Datasets, RVFDataset, RVFDatum, ScaleRange}
import edu.arizona.sista.struct.Counter

object DatasetNormalization {
  /** The actual scaling formula taken from svm-scale
    * Copied from edu.arizona.sista.learning.Datasets
    * */
  private def scale(value:Double, min:Double, max:Double, lower:Double, upper:Double):Double = {
    if(min == max) return upper

    // the result will be a value in [lower, upper]
    lower + (upper - lower) * (value - min) / (max - min)
  }

  /**
    * Scale each [[RVFDatum]] in an [[RVFDataset]] in place according to its own maximum feature value
    * @param dataset dataset to be normalized (in place)
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
    * Scale each feature in an [[RVFDataset]] in place according to the minimum and maximum values of the feature
    * @param dataset dataset to be normalized (in place)
    * @param lower lower bound of normalized values (e.g. -1 or 0), inclusive
    * @param upper upper bound of normalized values (e.g. 1), inclusive
    * @tparam L label type
    * @tparam F feature type
    */
  def scaleByFeature[L,F](dataset:RVFDataset[L,F], lower:Double, upper:Double): Unit = {
    Datasets.svmScaleDataset(dataset, lower, upper)
  }
}

