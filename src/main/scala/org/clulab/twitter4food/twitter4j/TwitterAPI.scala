package org.clulab.twitter4food.twitter4j

import org.clulab.twitter4food.struct.{Tweet, TwitterAccount}
import twitter4j.{Paging, Status, Twitter, TwitterFactory, TwitterException}
import twitter4j.User
import twitter4j.conf.ConfigurationBuilder

/**
  * Wrapper for Twitter4J
  * User: mihais
  * Date: 12/14/15
  */

/**
  * OAuth thru Food4Twitter app on Twitter
  * Owner: @nlpcpu and Dane Bell
  */

class TwitterAPI(keyset: Int) {

  val SLEEP = 5100
  val cb = new ConfigurationBuilder()
  val API_KEYS = "src/main/resources/org/clulab/twitter4food/twitter4j/APIKeys.txt"
  val keys = scala.io.Source.fromFile(API_KEYS)
                            .getLines.toList.slice(4*keyset, 4*(keyset+1))
                            .map(x => x.split("\t")(1))
  cb.setDebugEnabled(false)
    .setOAuthConsumerKey(keys(0))
    .setOAuthConsumerSecret(keys(1))
    .setOAuthAccessToken(keys(2))
    .setOAuthAccessTokenSecret(keys(3))

  val twitter = new TwitterFactory(cb.build()).getInstance

  def fetchAccount(handle: String, fetchTweets: Boolean = false,
                   fetchNetwork: Boolean = false): TwitterAccount = {
    var user: User = null
    try {
      user = twitter.showUser(handle)
    } catch {
      case te: TwitterException => println(te.getErrorCode + "\t" +
                                           te.getErrorMessage)
    }

    Thread.sleep(SLEEP)
    /** User is protected */
    if(user != null && user.getStatus != null) {
      val account = new TwitterAccount()

      account.setHandle(handle)
             .setId(user.getId)
             .setName(user.getName)
             .setLang(user.getLang)
             .setUrl(user.getURL)
             .setLocation(user.getLocation)
             .setDescription(user.getDescription)

      if(fetchTweets) {
        val tweets = twitter.getUserTimeline(handle, new Paging(1, 100))
                            .toArray(new Array[Status](0))
                            .map {
                              x => new Tweet(x.getText, x.getId,
                                             x.getLang, user.getScreenName)
                            }
        account.setTweets(tweets)
      }
      account
    }

    else null


    // TODO
  }


}
