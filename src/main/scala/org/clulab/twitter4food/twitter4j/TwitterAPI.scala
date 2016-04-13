package org.clulab.twitter4food.twitter4j

import org.clulab.twitter4food.struct.{Tweet, TwitterAccount}
import twitter4j._
import twitter4j.conf.ConfigurationBuilder

import scala.collection.mutable.{ArrayBuffer, Map, Set}
import scala.collection.JavaConverters._
import com.typesafe.config.ConfigFactory
import org.clulab.twitter4food.featureclassifier.HumanClassifier

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
  val QueryOnlySleepTime = 2050
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

  private val humanClassifier = new HumanClassifier(useUnigrams = true, useDictionaries = true)
//  private var isTrained = false

  // TODO: Convert to assertEquals from JUnit

  if(isAppOnly) 
    assert(twitter.getOAuth2Token().getTokenType() == "bearer")

  def sleep() = if(isAppOnly) Thread.sleep(AppOnlySleepTime) 
                else Thread.sleep(UserSleepTime)

  val option = (something: String) => if(something != null) something else ""
  val minId = (tweets: Seq[Status]) => tweets.foldLeft(Long.MaxValue)(
    (min, t) => if(t.getId < min) t.getId else min)

  def fetchAccount(handle: String, fetchTweets: Boolean = false,
                   fetchNetwork: Boolean = false): TwitterAccount = {
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
          var tweets = twitter.getUserTimeline(handle, page).asScala.toList
          sleep()

          while(!tweets.isEmpty) {
            tweetBuffer ++= tweets.map(x => new Tweet(option(x.getText), x.getId,
                                       option(x.getLang), x.getCreatedAt,
                                       user.getScreenName))
            val min = minId(tweets)

            page.setMaxId(min-1)
            tweets = twitter.getUserTimeline(handle, page).asScala.toList
            sleep()
            } 
        } catch {
          case te: TwitterException => print(s"ErrorCode = ${te.getErrorCode}\t")
                                          println(s"ErrorMsg = ${te.getErrorMessage}")
        }
      }

      var accounts = List[TwitterAccount]()

      if (fetchNetwork) {

        // Retrieve friends with bidirectional relationship as followers
        val followers = twitter.getFollowersIDs(id, 5000, 5000).getIDs // 5000 is maximum on cursor and count
        val activeFollowers = followers.filter(target => twitter.showFriendship(id, target).isTargetFollowedBySource)
        var numToRetrieve = 4
        var i = 0
        // Iterate over active followers
        while (numToRetrieve > 0 && i < activeFollowers.length) {
          // Get follower account
          val follower = twitter.showUser(activeFollowers(i))

          Thread.sleep(AccountSleepTime)

          // TODO classify as human or not

          if (follower != null && follower.getStatus != null) {
            // Recursively fetch the account, only getting tweets (NOT NETWORK)
            val toAdd = fetchAccount(follower.getScreenName, true, false)
            if (toAdd != null) {
              accounts = toAdd :: accounts
              numToRetrieve -= 1
            }
          }
          i += 1
        }
      }

      val account = new TwitterAccount(handle, id, name, language, url, 
        location, description, tweetBuffer.toSeq, accounts)

      account
    }
    else null
  }

  def search(keywords: Array[String]) = {
    val seenHandles = Set[String]()
    val results = Map[String, ArrayBuffer[Tweet]]()

    keywords foreach {
      k => {
        var query = new Query(k)
        query.setCount(100)
        query.setLang("en")
        try {
          var tweets = twitter.search(query).getTweets().asScala.toList
          
          Thread.sleep(QueryOnlySleepTime)

          while(!tweets.isEmpty) {
            tweets.foreach(q => {
              val handle = q.getUser.getScreenName
              if(!seenHandles.contains(handle)) {
                seenHandles += handle
                results += handle -> new ArrayBuffer[Tweet]()
                results(handle) += new Tweet(option(q.getText), q.getId, 
                  option(q.getLang), q.getCreatedAt, handle)
                }
              else results(handle) += new Tweet(option(q.getText), q.getId,
                option(q.getLang), q.getCreatedAt, handle)
              })
            val min = minId(tweets)
            query.setMaxId(min-1)

            tweets = twitter.search(query).getTweets().asScala.toList

            println(tweets.isEmpty)

            Thread.sleep(QueryOnlySleepTime)
          }
        } catch {
          case te: TwitterException => print(s"ErrorCode = ${te.getErrorCode}\t")
                                       println(s"ErrorMsg = ${te.getErrorMessage}")
        }
      }
    }
    results
  }
}
