package org.clulab.twitter4food.featureclassifier

import edu.arizona.sista.learning.{RVFDataset, RVFDatum, ScaleRange}
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

  def scaleByDatum[L, F](dataset:RVFDataset[L, F], lower:Double, upper:Double): Unit = {
    // scan the dataset once and keep track of min/max for each feature
    for(i <- dataset.indices) {
      val datumMin = dataset.values(i).min
      val datumMax = dataset.values(i).max
      for(j <- dataset.features(i).indices) {
        dataset.values(i)(j) = scale(dataset.values(i)(j), datumMin, datumMax, lower, upper)
      }
    }
  }

}

