package org.clulab.twitter4food.regression

import org.clulab.learning.{Classifier, LinearSVMClassifier, RVFDataset}
import org.clulab.struct.Counter
import org.clulab.twitter4food.struct.TwitterAccount

/**
  * Trait to be implemented by all feature regressions
  * User: mihais, dane
  * Date: 11/22/17
  */
trait Regression {
  def train(accounts: Seq[TwitterAccount], labels:Seq[String])

  def scoresOf(account: TwitterAccount): Counter[String]
}
