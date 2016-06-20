package edu.arizona.sista.twitter4food

import org.clulab.struct._

import Mixins._
import org.clulab.learning._
import scala.collection.immutable.IndexedSeq
import scala.collection.mutable.ArrayBuffer
import scala.collection.Set

/**
 * Created by dfried on 2/2/14.
 */

trait NormalizationType

case object NoNorm extends NormalizationType
case object DatumNorm extends NormalizationType
case object FeatureNorm extends NormalizationType

abstract class FeatureSelector[L, F] {
  def useFeature(f: F): Boolean
}

abstract class ThresholdScore[L, F](val data: Seq[Counter[F]],
                            val labels: Seq[L],
                            val numToRetain: Int,
                            val forceFeatures: Option[Set[F]] = None) extends FeatureSelector[L,F]{

  assert(numToRetain >= 1)

  val featureSet: Set[F] = data.map(_.keySet).reduce(_++_)

  val labelSet: Set[L] = labels.toSet

  val featureScores: Map[F, Double]

  lazy val cutoff = if (numToRetain < featureScores.size)
    featureScores.toSeq.sortBy(_._2).reverse(numToRetain - 1)._2
  else
    0

  def useFeature(f: F): Boolean = {
    (forceFeatures == None || forceFeatures.get.contains(f)) || (featureScores.contains(f) && featureScores(f) >=  cutoff)
  }

  def apply(counter: Counter[F]): Counter[F] = counter.filter({ case (f, x) => useFeature(f) })
}

class PointwiseMutualInformation[L, F](data: Seq[Counter[F]],
                                       labels: Seq[L],
                                       numToRetain: Int,
                                       forceFeatures: Option[Set[F]] = None) extends ThresholdScore[L, F](data,
  labels,  numToRetain, forceFeatures) {

  val labelCounts: Map[L, Int] = labels.groupBy(identity).mapValues(_.size)

  val N = data.size

  assert(labels.size == N)

  def pmi(f: F, l: L, countL: Int) = {
    val hasF: Counter[F] => Boolean = (_.keySet.contains(f))
    val countBoth = ((data zip labels).toStream filter {
      case (datum, l1) => l == l1 && hasF(datum)
      case _ => false
    }).size

    val countF = (data.toStream filter hasF).size
    // println(s"f: $f, l: $l")
    // println(s"countF: $countF, countL: $countL, countBoth: $countBoth")
    val ratio = countBoth * N.toFloat / (countF * countL)
    // println(s"prob ratio: $ratio ")

    Math.log(ratio)
  }

  def averageMI(f: F) = {
    // average mutal information
    labelCounts.toSeq.map({
      case (l, countL) => countL.toFloat / N * pmi(f, l, countL)
    }).sum
  }

  lazy val featureScores = (for {
    feature <- featureSet.toSeq
  } yield (feature -> averageMI(feature))).toMap
}

class MutualInformation[L, F](data: Seq[Counter[F]],
                              labels: Seq[L],
                              numToRetain: Int,
                              forceFeatures: Option[Set[F]] = None) extends ThresholdScore[L, F](data, labels,  numToRetain, forceFeatures) {
  // aka information gain, in Yang's paper
  def entropy[O](outcomes: Seq[O]) = {
    val groupedOutcomes: Seq[Seq[O]] = outcomes.groupBy(identity).values.toSeq
    val probs = groupedOutcomes.map(_.size.toFloat / outcomes.size).toSeq
    -1 * probs.map(x => x * Math.log(x)).sum
  }

  def informationGain(f: F) = {
    // mutual information
    val groupedLabels = (data zip labels).groupBy({ case (datum, label) => datum.keySet.contains(f) }).mapValues(_
      .map(_._2))
    val (positiveLabels, negativeLabels) = (groupedLabels.get(true).getOrElse(List()),
      groupedLabels.get(false).getOrElse(List()))
    // calculate marginal probability of feature
    val p_f: Float = positiveLabels.size.toFloat / data.size
    val p_not_f: Float = negativeLabels.size.toFloat / data.size
    assert(Math.abs(1 - (p_f + p_not_f)) < 1e-6, "probabilities do not sum to 1")
    entropy(labels) - (p_f * entropy(positiveLabels) + p_not_f * entropy(negativeLabels))
  }

  lazy val featureScores = {
    println("calculating feature scores")
    val stuff = (for {
      feature <- featureSet.toSeq
    } yield (feature -> informationGain(feature))).toMap
    println("done")
    stuff
  }
}

class CounterProcessor[A](val exampleCounters: Seq[Counter[A]],
                          val normalizationType: NormalizationType = NoNorm,
                          val thresholdPercentage: Option[Double] = None,
                          val thresholdValue: Option[Double] = None) extends Serializable{

  assert(thresholdPercentage == None || thresholdValue == None)

  val normalizer: (Counter[A] => Counter[A]) = normalizationType match {
    case NoNorm => identity _
    case DatumNorm => (counter => counter / counter.values.max)
    case FeatureNorm => {
      val maxByFeature: Counter[A] = exampleCounters.reduce(_.zipWith(Math.max)(_))
      counter => (counter / Counter.withDefault(1.0)(maxByFeature)).filterValues(x => x != 0)
    }
  }

  val threshold: (Counter[A] => Counter[A]) ={
    thresholdPercentage match {
      case None => thresholdValue match {
        case None =>
          identity _
        case Some(k) => {
          counter => counter.filterValues(_ >= k)
        }
      }
      case Some(pct) => {
        val totalCounts = exampleCounters.reduce(_+_)
        val threshold = totalCounts.values.sum * pct
        counter => {
          val sstream = counter.sorted.toStream
          val cumulative = sstream.tail.scanLeft(sstream.head) {
            case ((_, s), (t, c)) => (t, s + c)
          }
          val newCounter = new Counter[A]
          for ((key, count) <- cumulative.takeWhile { case (t, c) => c <= threshold })
            newCounter.setCount(key, count)
          newCounter
        }
      }
    }
  }

  def apply(counter: Counter[A]): Counter[A] = normalizer(threshold(counter))

}

