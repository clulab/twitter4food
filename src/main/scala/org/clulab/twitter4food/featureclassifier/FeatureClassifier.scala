package org.clulab.twitter4food.featureclassifier

import edu.arizona.sista.struct.Counter
import org.clulab.twitter4food.struct.TwitterAccount

/**
  * Trait to be implemented by all feature classifiers
  * User: mihais
  * Date: 12/14/15
  */
trait FeatureClassifier {
  /** Training from a set of users */
  def train(users:Seq[TwitterAccount])

  /** Predicting the given feature (or a distribution of) for a given account */
  def classify(user:TwitterAccount):Counter[String]
}
