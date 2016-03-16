package org.clulab.twitter4food.twitter4j

import org.clulab.twitter4food.struct.{Tweet, TwitterAccount}
import twitter4j._
import twitter4j.conf.ConfigurationBuilder
import scala.collection.mutable.ArrayBuffer
import com.typesafe.config.ConfigFactory

/**
  * Wrapper for Twitter4J
  * User: mihais
  * Date: 12/14/15
  *
  * OAuth thru Food4Twitter app on Twitter
  * Owner: @nlpcpu and Dane Bell
  */

class TwitterAPI(keyset: Int, isAppOnly: Boolean) {

  val AccountSleepTime = 5050
  val AppOnlySleepTime = 3050
  val UserSleepTime = 5050
  val MaxTweetCount = 200
  val MaxPageCount = 16

  val config = ConfigFactory.load()
  val cb = new ConfigurationBuilder()
  val keysFilePath = config.getString("twitter4j.api_keys")
  val keys = scala.io.Source.fromFile(keysFilePath)
                            .getLines.toList.slice(4*keyset, 4*(keyset+1))
                            .map(x => x.split("\t")(1))
  /* Application-only OAuth */                            
  if(!isAppOnly) {
    cb.setDebugEnabled(false)
      .setOAuthConsumerKey(keys(0))
      .setOAuthConsumerSecret(keys(1))
      .setOAuthAccessToken(keys(2))
      .setOAuthAccessTokenSecret(keys(3))
  }
  
  else {
    cb.setApplicationOnlyAuthEnabled(true)
      .setDebugEnabled(false)
      .setOAuthConsumerKey(keys(0))
      .setOAuthConsumerSecret(keys(1))
  }

  val twitter = new TwitterFactory(cb.build()).getInstance

  // TODO: Convert to assertEquals from JUnit

  if(isAppOnly) 
    assert(twitter.getOAuth2Token().getTokenType() == "bearer")

  def sleep() = if(isAppOnly) Thread.sleep(AppOnlySleepTime) 
                else Thread.sleep(UserSleepTime)

  def fetchAccount(handle: String, fetchTweets: Boolean = false,
                   fetchNetwork: Boolean = false): TwitterAccount = {
    val option = (something: String) => if(something != null) something else ""
    var user: User = null
    try {
        user = twitter.showUser(handle)
    } catch {
        case te: TwitterException => print(s"ErrorCode = ${te.getErrorCode}\t")
                                   println(s"ErrorMsg = ${te.getErrorMessage}")
    }

    Thread.sleep(AccountSleepTime)

    /** User is protected */
    if(user != null && user.getStatus != null) {
      val id = user.getId
      val name = option(user.getName)
      val language = option(user.getLang)
      val url = option(user.getURL)
      val location = option(user.getLocation)
      val description = option(user.getDescription)

      var tweets : Seq[Tweet] = null
      val tweetBuffer = ArrayBuffer[Tweet]()

      if(fetchTweets) {
        try {
          val page = new Paging(1, MaxTweetCount)
          var tweets = twitter.getUserTimeline(handle, page)
                                .toArray(new Array[Status](0))
          sleep()

          while(!tweets.isEmpty) {
            tweetBuffer ++= tweets.map(x => new Tweet(option(x.getText), x.getId,
                                       x.getLang, x.getCreatedAt, 
                                       user.getScreenName))
            val minId = tweets.foldLeft(Long.MaxValue)((min, t) => 
              if(t.getId < min) t.getId else min)
              
            page.setMaxId(minId-1)
            tweets = twitter.getUserTimeline(handle, page)
                              .toArray(new Array[Status](0))
            sleep()
            } 
        } catch {
          case te: TwitterException => print(s"ErrorCode = ${te.getErrorCode}\t")
                                          println(s"ErrorMsg = ${te.getErrorMessage}")
        }
      }

      val account = new TwitterAccount(handle, id, name, language, url, 
        location, description, tweetBuffer.toSeq)

      account
    }
    else null
  }
}
