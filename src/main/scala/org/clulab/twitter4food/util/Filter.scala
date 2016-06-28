package org.clulab.twitter4food.util

import com.typesafe.config.ConfigFactory

object Filter {
  val config = ConfigFactory.load

  /**
    * Filters out [[String]]s that contain any tokens marked as spam indicators
    * @param tweets
    * @return
    */
  def spamFilter(tweets: Seq[String]): Seq[String] = {
    def spamWords = scala.io.Source.fromFile(config.getString("spam_words")).getLines.map(_.stripLineEnd).toSet
    val okTweets = for (tweet <- tweets) yield {
      if (tweet.split("[\\s\\#]+").toSet.intersect(spamWords).isEmpty) Some(tweet)
      else None
    }
    val ret = okTweets.flatten
    println(s"Eliminated ${tweets.length - ret.length} of ${tweets.length} (${(tweets.length - ret.length) / tweets.length})")
    ret
  }
}