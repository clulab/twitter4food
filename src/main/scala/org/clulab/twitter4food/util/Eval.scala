package org.clulab.twitter4food.util

import scala.collection.mutable.ArrayBuffer
import org.clulab.twitter4food.struct.TwitterAccount

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