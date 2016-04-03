package org.clulab.twitter4food.util

import org.clulab.twitter4food.twitter4j._
import org.clulab.twitter4food.struct._
import java.io._
import com.typesafe.config.ConfigFactory
import scala.reflect.ClassTag
import scala.collection.mutable.ArrayBuffer
import edu.arizona.sista.learning.LiblinearClassifier

object TestUtils {
  def init(keyset: Int, isAppOnly: Boolean) = {
    (new TwitterAPI(keyset, isAppOnly), ConfigFactory.load())
  }

  def loadHandles(fileName: String) = {
    scala.io.Source.fromFile(fileName).getLines.toList
      .foldLeft(Map[String, String]())(
        (map, line) => map + (line.split("\t")(0) -> line.split("\t")(1)))
  }

  def splitHandles[T: ClassTag](keyset: Int, numWindows: Int, 
    collection: Map[T, String]): (Array[T], Array[String]) = {
    val window = collection.size/numWindows
    val subHandles = collection.keys.slice(keyset*window, (keyset+1)*window)
      .toArray
    val subLabels = subHandles.map(k => collection(k))

    (subHandles -> subLabels)
  }

  def fetchAccounts(api: TwitterAPI, handles: Seq[String], 
    fetchTweets: Boolean) = {
    handles.map(h => api.fetchAccount(h, fetchTweets))
  }

  def parseArgs(args: Array[String]) = {
    case class Config(useUnigrams: Boolean = false, 
      useBigrams: Boolean = false,
      useTopics: Boolean = false,
      useDictionaries: Boolean = false,
      useEmbeddings: Boolean = false)

    val parser = new scopt.OptionParser[Config]("classifier") {
      head("classifier", "0.x")
      opt[Unit]('u', "unigrams") action { (x, c) =>
        c.copy(useUnigrams = true)} text("use unigrams")
      opt[Unit]('b', "bigrams") action { (x, c) =>
        c.copy(useBigrams = true)} text("use bigrams")
      opt[Unit]('t', "topics") action { (x, c) =>
        c.copy(useTopics = true)} text("use topics")                
      opt[Unit]('d', "dictionaries") action { (x, c) =>
        c.copy(useDictionaries = true)} text("use dictionaries")
      opt[Unit]('e', "embeddings") action { (x, c) =>
        c.copy(useEmbeddings = true)} text("use embeddings")
    }

    parser.parse(args, Config()).get
  }

  def analyze(c: LiblinearClassifier[String, String], labels: Set[String],
    test: TwitterAccount, args: Array[String]) = {
    val params = parseArgs(args)
    val featureExtractor = new FeatureExtractor(params.useUnigrams, 
      params.useBigrams, params.useTopics,  params.useDictionaries,
      params.useEmbeddings)
    val W = c.getWeights()
    val d = featureExtractor.mkDatum(test, "unknown")

    val topWeights = labels.foldLeft(Map[String, Seq[(String, Double)]]())(
      (map, l) => map + (l -> W.get(l).get.toSeq.sortWith(_._2 > _._2)))

    val dotProduct = labels.foldLeft(Map[String, Seq[(String, Double)]]())(
      (map, l) => {
        val weightMap = W.get(l).get.toSeq.toMap
        val feats = d.featuresCounter.toSeq
        map + (l -> feats.filter(f => weightMap.contains(f._1))
          .map(f => (f._1, f._2 * weightMap(f._1))).sortWith(_._2 > _._2))
        })

    (topWeights, dotProduct)
  }

  def analyze(filename: String, labels: Set[String], test: TwitterAccount,
    args: Array[String]) = {
    analyze(LiblinearClassifier.loadFrom[String, String](filename), labels,
      test, args)
  }
}

class EvalMetric {
  var TP = 0
  var FP = 0
  var FN = 0
  var TN = 0
  def P = TP.toFloat/(TP+FP)
  def R = TP.toFloat/(TP+FN)
  def A = (TP+TN).toFloat/(TP+FP+TN+FN)
  val TPAccounts = ArrayBuffer[TwitterAccount]()
  val FPAccounts = ArrayBuffer[TwitterAccount]()
  val TNAccounts = ArrayBuffer[TwitterAccount]()
  val FNAccounts = ArrayBuffer[TwitterAccount]()
  var beta = 0.0
  var F = Eval.fMeasure(P, R, beta)
  val df = new java.text.DecimalFormat("#.###")
  
  override def toString = {
    s"(P: ${df.format(P)}\tR: ${df.format(R)}\tF-1: ${df.format(F)}\t" +
    s"A: ${df.format(A)})" 
  }
} 

object Eval {

  def fMeasure(precision: Double, recall: Double, beta: Double): Double =
    (1 + Math.pow(beta, 2)) * ((precision * recall) / 
    (Math.pow(beta, 2) * precision + recall))

  def genEvalMeasure(labels: Set[String]) = {
    labels.foldLeft(Map[String, EvalMetric]())(
      (m, l) => m + (l -> new EvalMetric()))
  }  

  def evaluate(srcLabels: Seq[String], predictedLabels: Seq[String],
    accounts: Seq[TwitterAccount]) = {
    val labels = srcLabels.toSet
    val evalMeasures = genEvalMeasure(labels)
    assert(srcLabels.size == predictedLabels.size
      && srcLabels.size == accounts.size)

    labels.foreach(label => {
      val eval = evalMeasures(label)
      for(i <- srcLabels.indices) {
        if(srcLabels(i) equals label) {
          if(predictedLabels(i) equals label) {
              eval.TP += 1
              eval.TPAccounts += accounts(i)
            } 
          else {
            eval.FN += 1
            eval.FNAccounts += accounts(i)
          }
        }
        else {
          if(predictedLabels(i) equals label) {
            eval.FP += 1
            eval.FPAccounts += accounts(i)
          }
          else {
            eval.TN += 1
            eval.TNAccounts += accounts(i)
          }
        }
      }
      eval.F = fMeasure(eval.P, eval.R, 1.0)
    })

    // Macroaverage
    val macroAvg = evalMeasures.values.foldLeft(0.0)(
      (sum, e) => sum + e.F)/evalMeasures.size

    // Microaverage
    val e = new EvalMetric()
    val sumE = evalMeasures.values.foldLeft(e)(
      (s, e) => { s.TP += e.TP; s.FP += e.FP; s.FN += e.FN; s})
    val microAvg = Eval.fMeasure(sumE.P, sumE.R, 1.0)

    (evalMeasures, macroAvg, microAvg)
  }
}