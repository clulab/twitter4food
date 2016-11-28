package org.clulab.twitter4food.struct

import org.clulab.struct.{Counter, Lexicon}

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/**
  * Like a dataset in that it has multiple rows, but like a datum in that it has
  * a single set of labels
  */
trait MimlDatum[L,F] {
  val labelLexicon: Lexicon[L]
  val featureLexicon: Lexicon[F]
  val posLabels: Set[Int]
  val negLabels: Set[Int]
  def += (instance: Instance[L, F])
}

/**
  * Datum with instances having binary-valued features
  */
class BMimlDatum[L, F](
  val labelLexicon: Lexicon[L],
  val featureLexicon: Lexicon[F],
  val posLabels: Set[Int],
  val negLabels: Set[Int],
  val features: ArrayBuffer[Array[Int]]
) extends MimlDatum[L, F] {

  def this() = this(
    new Lexicon[L],
    new Lexicon[F],
    Set[Int](),
    Set[Int](),
    new ArrayBuffer[Array[Int]])

  override def += (instance: Instance[L, F]) = {
    instance match {
      case bi:BVFInstance[L, F] =>
        features += featuresToArray(bi.features)
      case _ => throw new RuntimeException("ERROR: you cannot add a non BVFInstanceto a MimlBDatum!")
    }
  }

  private def featuresToArray(fs:Iterable[F]):Array[Int] = {
    val fb = new ListBuffer[Int]
    for(f <- fs) fb += featureLexicon.add(f)
    fb.toList.sorted.toArray
  }
}

/**
  * Datum with instances having real-valued features
  */
class RMimlDatum[L, F](
  ll: Lexicon[L],
  fl: Lexicon[F],
  pls: Set[Int],
  nls: Set[Int],
  fs: ArrayBuffer[Array[Int]],
  val values: ArrayBuffer[Array[Double]]) extends BMimlDatum[L, F](ll, fl, pls, nls, fs) {

  def this() = this(
    new Lexicon[L],
    new Lexicon[F],
    Set[Int](),
    Set[Int](),
    new ArrayBuffer[Array[Int]],
    new ArrayBuffer[Array[Double]])

  override def += (instance: Instance[L, F]) = {
    instance match {
      case i:RVFInstance[L, F] =>
        val (fs, vs) = featuresCounterToArray(i.featuresCounter).unzip
        features += fs
        values += vs
      case _ => throw new RuntimeException("ERROR: you cannot add a non RVFDatum to a RVFDataset!")
    }
  }

  private def featuresCounterToArray(fs: Counter[F]): Array[(Int, Double)] = {
    val fb = new ListBuffer[(Int, Double)]
    for(f <- fs.keySet) {
      fb += new Tuple2[Int, Double](featureLexicon.add(f), fs.getCount(f))
    }
    fb.sortBy(_._1).toArray
  }

}
