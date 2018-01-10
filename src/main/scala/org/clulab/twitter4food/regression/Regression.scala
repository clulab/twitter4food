package org.clulab.twitter4food.regression

import org.clulab.struct.Counter
import org.clulab.twitter4food.struct.TwitterAccount

/**
  * Trait to be implemented by all feature regressions
  * User: mihais, dane
  * Date: 11/22/17
  */
trait Regression {
  def train(accounts: Seq[TwitterAccount], labels:Seq[Double])

  def scoreOf(account: TwitterAccount): Counter[String]
}
