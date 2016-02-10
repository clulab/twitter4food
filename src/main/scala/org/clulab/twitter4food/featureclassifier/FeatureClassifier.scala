package org.clulab.twitter4food.featureclassifier

import edu.arizona.sista.learning.{Classifier, LinearSVMClassifier, RVFDataset}
import edu.arizona.sista.struct.Counter
import org.clulab.twitter4food.struct.TwitterAccount

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map

/**
  * Trait to be implemented by all feature classifiers
  * User: mihais
  * Date: 12/14/15
  */
trait FeatureClassifier {

  /**
    * Training from a set of users
    * Creates the subClassifier object as the output
    */
  def train(accounts: Seq[TwitterAccount], labels:Seq[String])

  /** Predicting the given feature (or a distribution of) for a given account */
  def classify(account: TwitterAccount): String = {
    val scores = scoresOf(account)
    scores.sorted.head._1
  }

  def scoresOf(account: TwitterAccount): Counter[String]

}
