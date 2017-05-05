package org.clulab.twitter4food.twitter4j

import org.clulab.twitter4food.struct.{Tweet, TwitterAccount, Location}
import twitter4j._
import twitter4j.conf.ConfigurationBuilder

import scala.collection.mutable.ArrayBuffer
import scala.collection.JavaConverters._
import com.typesafe.config.ConfigFactory

/**
  * Wrapper for Twitter4J
  * User: mihais
  * Date: 12/14/15
  *
  * OAuth thru Food4Twitter app on Twitter
  * Owner: @nlpcpu and Dane Bell
  */

class TwitterAPI(keyset: Int) {

  val AccountSleepTime = 5050
  val AppOnlySleepTime = 3050
  val QueryOnlySleepTime = 2050
  val UserSleepTime = 5050
  val MaxTweetCount = 200
  val MaxPageCount = 16

  private val UserOnly = 0
  private val AppOnly = 1

  private val config = ConfigFactory.load()
  private val cb1 = new ConfigurationBuilder()
  private val cb2 = new ConfigurationBuilder()
  private val keysFilePath = config.getString("twitter4j.api_keys")
  private val keys = scala.io.Source.fromFile(keysFilePath)
                            .getLines.toList.slice(4*keyset, 4*(keyset+1))
                            .map(x => x.split("\t")(1))

  private val RateLimitChart = scala.collection.Map("showUser" -> Array(180, 180), "getUserTimeline" -> Array(180, 300),
    "getFollowersIDs" -> Array(15, 15), "getFriendsIDs" -> Array(15, 15), "showFriendship" -> Array(180, 15),
    "search" -> Array(180, 450), "lookupUsers" -> Array(180, 60))

  System.setProperty("twitter4j.loggerFactory", "twitter4j.NullLoggerFactory")
  /* User-only OAuth */
  cb1.setDebugEnabled(false)
    .setOAuthConsumerKey(keys(0))
    .setOAuthConsumerSecret(keys(1))
    .setOAuthAccessToken(keys(2))
    .setOAuthAccessTokenSecret(keys(3))

  val userOnlyTwitter = new TwitterFactory(cb1.build()).getInstance

  /* Application-only OAuth */
  cb2.setApplicationOnlyAuthEnabled(true)
    .setDebugEnabled(false)
    .setOAuthConsumerKey(keys(0))
    .setOAuthConsumerSecret(keys(1))

  val appOnlyTwitter = new TwitterFactory(cb2.build()).getInstance

  // TODO use human classifier to predict if active followers are human (is this necessary?????)
  //  private val humanClassifier = new HumanClassifier(useUnigrams = true, useDictionaries = true)
  //  private var isTrained = false

  assert(appOnlyTwitter.getOAuth2Token().getTokenType() == "bearer")

  private def calcSleepTime(numGetsPer15: Int) = (60*15/numGetsPer15 * 1000)+20

  private def sleep(method: String, isAppOnly: Boolean = true) = {
    val DEBUG = false
    val idx = if(isAppOnly) AppOnly else UserOnly
    val sleepTime = calcSleepTime(RateLimitChart(method)(idx))

    if(DEBUG) println(s"$method call sleeping for ${sleepTime/1000.0} seconds")
    Thread.sleep(sleepTime)
  }

  private val option = (something: String) => if(something != null) something else ""
  private val minId = (tweets: Seq[Status]) => tweets.foldLeft(Long.MaxValue)(
    (min, t) => if(t.getId < min) t.getId else min)
  private val sanitizeHandle = (h: String) => if(h(0) == '@') h else "@"+h

  def fetchAccount(h: String, fetchTweets: Boolean = false,
                   fetchNetwork: Boolean = false, isID: Boolean = false,
                   isAppOnly: Boolean = true): TwitterAccount = {
    val twitter = if(isAppOnly) appOnlyTwitter else userOnlyTwitter
    val _handle = if(!isID) sanitizeHandle(h) else h
    var user: User = null
    try {
      if (isID)
        user = twitter.showUser(_handle.toLong)
      else
        user = twitter.showUser(_handle)
    } catch {
        case te: TwitterException => print(s"ErrorCode = ${te.getErrorCode}\t")
                                   println(s"ErrorMsg = ${te.getErrorMessage}")
    }

    sleep("showUser", isAppOnly)

    /** User is protected */
    if(user != null && user.getStatus != null) {
      val handle = sanitizeHandle(user.getScreenName)
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
          sleep("getUserTimeline", isAppOnly)

          while(!tweets.isEmpty) {
            tweetBuffer ++= tweets.map(x => new Tweet(option(x.getText), x.getId,
                                       option(x.getLang), x.getCreatedAt,
                                       sanitizeHandle(user.getScreenName)))
            val min = minId(tweets)

            page.setMaxId(min-1)
            tweets = twitter.getUserTimeline(handle, page).asScala.toList
            sleep("getUserTimeline", isAppOnly)
            } 
        } catch {
          case te: TwitterException => print(s"ErrorCode = ${te.getErrorCode}\t")
                                       println(s"ErrorMsg = ${te.getErrorMessage}")
        }
      }

      var followers = List[TwitterAccount]()

      if (fetchNetwork) {
        // Query the user. Sort by decreasing number of interactions
        val candidates = search(Array(handle)).toSeq
          .sortWith(_._2.size > _._2.size).map(_._1)

        // Sleep for user-only sleep time
        println(s"Checking for follower status")
        val addActiveFollowers = (_count: Int, users: Seq[String],
          isID: Boolean) => {

          // Iterate over activeUsers and followers.
          var i = 0
          var count = _count
          while (count > 0 && i < users.length) {
            // Get follower account
            // Recursively fetch the account, only getting tweets (NOT NETWORK)
            val tgt = users(i)
            var condition = false
            try {
              condition = if(isID) userOnlyTwitter.showFriendship(id, tgt.toLong)
                .isTargetFollowedBySource
                else userOnlyTwitter.showFriendship(handle, tgt)
                  .isTargetFollowedBySource

              sleep("showFriendship", isAppOnly=false)

            } catch {
              case te: TwitterException => print(s"ErrorCode = ${te.getErrorCode}\t")
                                           println(s"ErrorMsg = ${te.getErrorMessage}")
            }

            val follower = if(condition) fetchAccount(tgt,
              fetchTweets=true, fetchNetwork=false, isID)
              else null
            
            if(follower != null) {
              followers = follower :: followers
              count -= 1
            }
            i += 1
          }
        }

        val numToRetrieve = 4
        addActiveFollowers(numToRetrieve, candidates, false)

        if(followers.size < 4) {
          // Not enough mentions. Fall back on followers
          // Retrieve friends with bidirectional relationship as followers
          // 5000 is maximum on cursor and count
          try {
            val _followerIDs = addActiveFollowers(numToRetrieve - followers.size,
            twitter.getFollowersIDs(id, -1, 5000).getIDs.map(_.toString), true)
            } catch {
              case te: TwitterException => print(s"ErrorCode = ${te.getErrorCode}\t")
                                       println(s"ErrorMsg = ${te.getErrorMessage}")
            }
          sleep("getFollowersIDs", isAppOnly) //one minute per getFollowers
        }
      }

      val account = new TwitterAccount(handle, id, name, language, url, 
        location, description, tweetBuffer, followers)

      account
    }
    else null
  }

  def search(keywords: Array[String], isAppOnly: Boolean = true) = {
    val seenHandles = scala.collection.mutable.Set[String]()
    val results = scala.collection.mutable.Map[String, ArrayBuffer[Tweet]]()
    val twitter = if(isAppOnly) appOnlyTwitter else userOnlyTwitter

    keywords foreach {
      k => {
        val query = new Query(k)
        query.setCount(100)
        query.setLang("en")
        try {
          var tweets = twitter.search(query).getTweets().asScala.toList
          
          sleep("search", isAppOnly)

          while(tweets.nonEmpty) {
            tweets.foreach(q => {
              val handle = sanitizeHandle(q.getUser.getScreenName)
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

            sleep("search", isAppOnly)
          }
        } catch {
          case te: TwitterException => print(s"ErrorCode = ${te.getErrorCode}\t")
                                       println(s"ErrorMsg = ${te.getErrorMessage}")
        }
      }
    }
    results.map{ case (k,v) => (k, v.toArray) }
  }

  def fetchFolloweeHandles(h: String): Seq[String] = {
    val handle = sanitizeHandle(h)
    val ids = try {
      appOnlyTwitter.getFriendsIDs(handle, -1).getIDs.toSeq // first 5000 only
    } catch {
      case e: Exception => println(s"Account $handle not found!")
      Nil
    }
    sleep("getFriendsIDs", isAppOnly = true)
    val screenNames = ArrayBuffer[String]()
    val idLength = ids.length
    var i = 0
    while (i < idLength) {
      screenNames ++= appOnlyTwitter.lookupUsers(ids.slice(i, math.min(i+100, idLength)):_*).asScala.map(_.getScreenName)
      sleep("lookupUsers", isAppOnly = true)
      i += 100
    }
    screenNames
  }

  def fetchProfilePic(h: String): Option[String] ={
    val handle = sanitizeHandle(h)
    val user = try {
      Option(appOnlyTwitter.showUser(handle))
    } catch {
      case te: TwitterException =>
        println(s"ErrorCode = ${te.getErrorCode}\tErrorMsg = ${te.getErrorMessage}")
        None
    }
    sleep("showUser", isAppOnly = true)

    if (user.nonEmpty) {
      if (user.get.isDefaultProfileImage) Option("default") else Option(user.get.getOriginalProfileImageURL)
    } else None
  }

  def fetchImages(id: Long): (Seq[String], Seq[String]) = {
    val mediaBuffer = ArrayBuffer[String]()
    val urlBuffer = ArrayBuffer[String]()

    try {
      val page = new Paging(1, MaxTweetCount)
      var tweets = appOnlyTwitter.getUserTimeline(id, page).asScala.toList
      sleep("getUserTimeline")

      while(tweets.nonEmpty) {
        mediaBuffer ++= tweets.flatMap{t =>
          val mes = t.getMediaEntities.map(me => me.getMediaURLHttps)
          val emes = t.getExtendedMediaEntities.filter(eme => eme.getType == "photo").map(_.getMediaURLHttps)
          mes ++ emes
        }

        urlBuffer ++= tweets.flatMap(t =>
          t.getURLEntities
            .map(ent => ent.getExpandedURL)
            .filter(
              link => link.contains("://instagram.com/p/") ||
              link.contains("://instagr.am/p/")
            )
        )

        page.setMaxId(minId(tweets) - 1)

        tweets = appOnlyTwitter.getUserTimeline(id, page).asScala.toList
        sleep("getUserTimeline")
      }

    } catch {
      case te: TwitterException => print(s"ErrorCode = ${te.getErrorCode}\t")
        println(s"ErrorMsg = ${te.getErrorMessage}")
    }

    (mediaBuffer, urlBuffer)
  }

  def fetchCoords(id: Long): Seq[Location] = {
    val coords = ArrayBuffer[Location]()

    try {
      val page = new Paging(1, MaxTweetCount)
      var tweets = appOnlyTwitter.getUserTimeline(id, page).asScala.toList
      sleep("getUserTimeline")

      while(tweets.nonEmpty) {
        coords ++= tweets.flatMap{t =>
          val gl = t.getGeoLocation
          if (gl != null)
            Option(new Location(t.getId.toString, gl.getLatitude, gl.getLongitude, id, t.getCreatedAt, "twitter", Nil))
          else None
        }

        page.setMaxId(minId(tweets) - 1)

        tweets = appOnlyTwitter.getUserTimeline(id, page).asScala.toList
        sleep("getUserTimeline")
      }

    } catch {
      case te: TwitterException => print(s"ErrorCode = ${te.getErrorCode}\t")
        println(s"ErrorMsg = ${te.getErrorMessage}")
    }

    coords
  }

}
