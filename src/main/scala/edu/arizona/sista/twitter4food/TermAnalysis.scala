package edu.arizona.sista.twitter4food

import edu.arizona.sista.struct._
import Mixins._
import scala.collection._
import java.io.{OutputStream, PrintStream, File, PrintWriter}
import scala.io.Source
import jp.sf.amateras.solr.scala.QueryBuilder
import scala.{Predef, collection}
import scala.Some
import scala.Iterator
import scala.Seq
import scala.Predef.Set
import scala.BufferedIterator
import scala.Iterable
import collection.JavaConversions._

/**
 * Created by dfried on 4/16/14.
 */
object TermAnalysis {

  def tfsFromIterator[O, L](labelFn: O => Option[L])(docFn: O => Seq[String])(objects: Iterator[O]):
  Map[L, Counter[String]] = {
    val tfs = new collection.mutable.HashMap[L, Counter[String]]

    for (o <- objects) {
      val document = docFn(o)
      labelFn(o) match {
        case Some(l) => {
          if (!tfs.contains(l)) tfs += ((l, new Counter[String]()))
          val sc = tfs(l)
          for (token <- document) {
            // get rid of hashtags
            val stripToken = if (token.charAt(0) == '#') token.substring(1) else token
            if (okTerm(stripToken)) sc.incrementCount(stripToken)
          }
        }
        case None => ()
      }
    }
    tfs.toMap
  }

  def getLoc(m: Tweet): Option[String] =
    m.normalizedLocation.toStringOpt

  def getDoc(m: Tweet): Seq[String] = if (m.tokens == null) List() else m.tokens.toList

  private def solrTweets: Stream[Tweet] = {
    val solrPager = (new SolrPager)
    solrPager.pagedRequest(solrPager.client.query("normalizedLocation:[* TO *]"))
  }

  def tfsFromSolr: Map[String, Counter[String]] = tfsFromIterator(getLoc)(getDoc)(solrTweets.toIterator)

  def tfsFromSolrGrouped[G](groupFn: Tweet => G): Map[G, Map[String, Counter[String]]] =
    solrTweets.groupBy(groupFn).mapValues(stream => tfsFromIterator(getLoc)(getDoc)(stream.toIterator))

  def mkTfIdfs[L](tfThreshold: Int)(tfs: Map[L, Counter[String]]): Map[L, Counter[String]] = {
    // compute idfs, treating states as documents
    var docCount = new Counter[String]()
    for (doc <- tfs.values)
      for (term <- doc.keySet)
        docCount.incrementCount(term)

    val N = tfs.size // should be 50 or 51 or 52, idk

    val idfs: Counter[String] = docCount.mapValues(count => Math.log(N / count))

    tfs.mapValues(cntr => (cntr.filterValues(_ >= tfThreshold) * idfs).filterValues(_ > 0))
  }

  val ONLY_FOOD_WORDS = true

  val ONLY_TF  = false
  val MOD_PMIS = true

  def okTerm(term: String) = {
    if (ONLY_FOOD_WORDS) FoodWords.words.contains(term)
    else !term.startsWith("@") && !term.startsWith("http")
  }

  def mkModPmis[L](tfsByState: Map[L, Counter[String]]): Map[L, Counter[String]] = {
    val totalCounts = tfsByState.values.reduce(_ + _)
    val n = totalCounts.values.sum
    tfsByState.mapValues(tf => {
      val totalPos = tf.values.sum
      val newC = new Counter[String]
      for ((word, a) <- tf.toSeq) {
        val b = totalCounts.getCount(word)
        val c = totalPos - a
        val pmi = math.log((a * n) / ((a + c) * (a + b)))
        newC.setCount(word, pmi * math.log(a))
      }
      newC
    })
  }

  def toJsonNested[R <% Ordered[R], S](tf_idf: Map[R, Map[S, Counter[String]]]) = {
    JacksonWrapper.serialize(tf_idf.mapValues(_.mapValues(_.sorted.take(50))).toSeq.sortBy(_._1))
  }

  def toJson(tf_idf: Map[String, Counter[String]]): String = {
    JacksonWrapper.serialize(tf_idf.mapValues(counter => counter.sorted.take(20)))
  }

  def iterativeGroupBy[T, B](iter0: Iterator[T])(func: T => B): Iterator[(B, Iterator[T])] = new Iterator[(B, Iterator[T])] {
    var iter: BufferedIterator[T] = iter0.buffered

    def hasNext = iter.hasNext

    def next = {
      val first = iter.head
      val firstValue = func(first)
      val (i1: Iterator[T], i2: Iterator[T]) = iter.span(el => func(el) == firstValue)
      iter = i2.buffered
      (firstValue, i1)
    }
  }

  def identity[A](a: A): A = a

  def main(args: Array[String]) = {
    // output streams
    val err: PrintStream = System.err
    System.setErr(new PrintStream(new OutputStream() {
      def write(b: Int) = {}
    }))

    val fn: (Map[String, Counter[String]]) => Map[String, Counter[String]] =
      if (ONLY_TF) identity else if (MOD_PMIS) (mkModPmis[String] _) else (mkTfIdfs[String](0) _)

    val global = Map(("all", fn(tfsFromSolr)))

    val p1: PrintWriter = (if (args.length > 0) new PrintWriter(new File(args(0))) else new PrintWriter(System.out))
    p1.println(toJsonNested(global))
    p1.close()

    val monthly = tfsFromSolrGrouped(tweet => {
      val postTime: java.util.Date = tweet.postTime
      (postTime.getYear + 1900, postTime.getMonth + 1)
    }).mapValues(fn)

    val p2: PrintWriter = (if (args.length > 1) new PrintWriter(new File(args(1))) else new PrintWriter(System.out))
    p2.println(toJsonNested(monthly))
    p2.close()

    System.setErr(err)
  }
}
