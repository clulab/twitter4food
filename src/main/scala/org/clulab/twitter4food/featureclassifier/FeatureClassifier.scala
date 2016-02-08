package org.clulab.twitter4food.featureclassifier

import edu.arizona.sista.learning.{LinearSVMClassifier, RVFDataset}
import org.clulab.twitter4food.struct.TwitterAccount

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map

/**
  * Trait to be implemented by all feature classifiers
  * User: mihais
  * Date: 12/14/15
  */
trait FeatureClassifier {

  // TODO: Figure out how to implement traits within traits in subclasses
  // TODO: ^for attributes subClassifier and labels

  var dataset: RVFDataset[String, String]
  var subClassifier: LinearSVMClassifier[String, String]
  var labels: List[String]
  var trainingLabels: Map[String, String]
  var trainingSet: ArrayBuffer[TwitterAccount]

  /** Training from a set of users */
  def train(users: Seq[TwitterAccount])

  /** Use default training set */
  def train()

  /** Predicting the given feature (or a distribution of) for a given account */
  def classify(user: TwitterAccount): String

  /** Assign labels for different features predicted */
  def assignLabels(users: Seq[TwitterAccount])

  /** Assign labels to default training set */
  def assignLabels()
}
