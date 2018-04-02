package org.clulab.twitter4food.util

import scala.collection.mutable.ArrayBuffer
import org.clulab.twitter4food.struct.TwitterAccount
import org.slf4j.LoggerFactory

class EvalMetric {
  var TP = 0
  var FP = 0
  var FN = 0
  var TN = 0
  def P = if ((TP + FP) == 0) 0.0 else TP.toDouble/(TP + FP)
  def R = if ((TP + FN) == 0) 0.0 else TP.toDouble/(TP + FN)
  def A = if ((TP + FP + TN + FN) == 0) 0.0 else (TP + TN).toDouble/(TP + FP + TN + FN)
  val TPAccounts = ArrayBuffer[TwitterAccount]()
  val FPAccounts = ArrayBuffer[TwitterAccount]()
  val TNAccounts = ArrayBuffer[TwitterAccount]()
  val FNAccounts = ArrayBuffer[TwitterAccount]()
  var beta = 0.0
  var F = Eval.fMeasure(P, R, beta)
  val df = new java.text.DecimalFormat("#.###")

  override def toString = f"P: $P%1.5f\tR: $R%1.5f\tF1: $F%1.5f\tA: $A%1.5f"
}

object Eval {

  val logger = LoggerFactory.getLogger(this.getClass)

  def fMeasure(precision: Double, recall: Double, beta: Double): Double = {
    if ((Math.pow(beta, 2) * precision + recall) == 0.0)
      0.0
    else
      (1 + Math.pow(beta, 2)) * ((precision * recall) /
        (Math.pow(beta, 2) * precision + recall))
  }

  def genEvalMeasure(labels: Set[String]): Map[String, EvalMetric] = {
    labels.foldLeft(Map[String, EvalMetric]())(
      (m, l) => m + (l -> new EvalMetric()))
  }

  def f1ForLabel(label: String)(labels: Iterable[(String, String)]): Double = {
    val (evalMeasures, _, _) = evaluate(labels.toSeq)
    evalMeasures(label).F
  }

  def macroOnly(labels: Iterable[(String, String)]): Double = {
    evaluate(labels.toSeq)._2
  }

  def macroOnly(gold: Seq[String], pred: Seq[String]): Double = {
    evaluate(gold, pred)._2
  }

  def microOnly(labels: Iterable[(String, String)]): Double = {
    evaluate(labels.toSeq)._3
  }

  def microOnly(gold: Seq[String], pred: Seq[String]): Double = {
    evaluate(gold, pred)._3
  }

  def evaluate(labels: Seq[(String, String)]): (Map[String, EvalMetric], Double, Double) = {
    val (srcLabels, predictedLabels) = labels.unzip
    evaluate(srcLabels, predictedLabels)
  }

  def evaluate(srcLabels: Seq[String], predictedLabels: Seq[String]): (Map[String, EvalMetric], Double, Double) = {
    val labels = srcLabels.toSet

    val evalMeasures = genEvalMeasure(labels)

    labels.foreach(label => {
      val eval = evalMeasures(label)
      for (i <- srcLabels.indices) {
        if (srcLabels(i) equals label) {
          if (predictedLabels(i) equals label) eval.TP += 1 else eval.FN += 1
        }
        else {
          if (predictedLabels(i) equals label) eval.FP += 1 else eval.TN += 1
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

  def evaluate(srcLabels: Seq[String], predictedLabels: Seq[String],
               accounts: Seq[TwitterAccount]): (Map[String, EvalMetric], Double, Double) = {
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
      (s, e) => {
        s.TP += e.TP
        s.FP += e.FP
        s.FN += e.FN
        s
      }
    )
    val microAvg = Eval.fMeasure(sumE.P, sumE.R, 1.0)

    (evalMeasures, macroAvg, microAvg)
  }
}