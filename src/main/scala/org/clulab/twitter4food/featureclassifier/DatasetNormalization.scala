package org.clulab.twitter4food.featureclassifier

import edu.arizona.sista.learning.{RVFDataset, RVFDatum}
import edu.arizona.sista.struct.Counter

abstract class DatasetNormalization {
  def normalize[L,F](dataset: RVFDataset[L,F]): RVFDataset[L,F]
}

class NoNormalization extends DatasetNormalization {
  def normalize[L,F](dataset: RVFDataset[L,F]): RVFDataset[L,F] = dataset
}

class DatumNormalization extends DatasetNormalization {
  def labelledFeaturesCounter[L,F](dataset: RVFDataset[L,F], datumOffset:Int):Counter[F] = {
    val c = new Counter[F]
    val fs = dataset.features(datumOffset).map(l => dataset.featureLexicon.get(l))
    val vs = dataset.values(datumOffset)
    for(i <- fs.indices) {
      c.incrementCount(fs(i), vs(i))
    }
    c
  }

  def normalize[L,F](dataset: RVFDataset[L,F]): RVFDataset[L,F] = {
    val normalized = new RVFDataset[L,F]()
    for (i <- dataset.indices) {
      val fc = labelledFeaturesCounter(dataset, i)
      val normed = fc / fc.l1Norm
      normalized += new RVFDatum[L,F](dataset.labelLexicon.get(dataset.labels(i)), normed)
    }
    normalized
  }
}


