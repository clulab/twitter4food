package org.clulab.twitter4food.struct

import org.clulab.struct.Counter

abstract class Instance[L, F] {
  def features: Iterable[F]
  def featuresCounter: Counter[F]
}

class BVFInstance[L, F](
  val features: Iterable[F]
) extends Instance[L, F] {
  def featuresCounter: Counter[F] = {
    val c = new Counter[F]
    for(f <- features) {
      c.incrementCount(f)
    }
    c
  }

  override def equals(other:Any):Boolean = {
    other match {
      case that:BVFInstance[L, F] => features == that.features
      case _ => false
    }
  }

  override def hashCode = features.hashCode()

}

class RVFInstance[L, F](val featuresCounter: Counter[F]) extends Instance[L, F] {

  def features = featuresCounter.keySet

  def getFeaturesCount(f: F) = featuresCounter.getCount(f)

  override def equals(other:Any):Boolean = {
    other match {
      case that:RVFInstance[L, F] => featuresCounter == that.featuresCounter
      case _ => false
    }
  }

}