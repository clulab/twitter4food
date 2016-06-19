package org.clulab.twitter4food.featureclassifier

import org.clulab.learning.{Classifier, LinearSVMClassifier, RVFDataset}
import org.clulab.struct.Counter
import org.clulab.twitter4food.struct.TwitterAccount

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map

/**
  * Trait to be implemented by all feature classifiers
  * User: mihais
  * Date: 12/14/15
  */
trait FeatureClassifier {

  def train(accounts: Seq[TwitterAccount], labels:Seq[String])

  def classify(account: TwitterAccount): String = {
    val scores = scoresOf(account)
    scores.sorted.head._1
  }

  def scoresOf(account: TwitterAccount): Counter[String]

}
