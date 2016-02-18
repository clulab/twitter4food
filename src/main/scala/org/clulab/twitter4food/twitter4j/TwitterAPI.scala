package org.clulab.twitter4food.twitter4j

import org.clulab.twitter4food.struct.{Tweet, TwitterAccount}
import twitter4j.{Paging, Status, TwitterFactory, TwitterException}
import twitter4j.User
import twitter4j.conf.ConfigurationBuilder

/**
  * Wrapper for Twitter4J
  * User: mihais
  * Date: 12/14/15
  *
  * OAuth thru Food4Twitter app on Twitter
  * Owner: @nlpcpu and Dane Bell
  */

class TwitterAPI(keyset: Int) {

    val sleepTime = 5100
    val cb = new ConfigurationBuilder()
    val keysFilePath = "src/main/resources/org/clulab/twitter4food/twitter4j/APIKeys.txt"
    val keys = scala.io.Source.fromFile(keysFilePath)
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

        Thread.sleep(sleepTime)

        /** User is protected */
        if(user != null && user.getStatus != null) {
            val id = user.getId
            val name = user.getName
            val language = user.getLang
            val url = user.getURL
            val location = user.getLocation
            val description = user.getDescription

            var tweets : Seq[Tweet] = null

            if(fetchTweets) {
                tweets = twitter.getUserTimeline(handle, new Paging(1, 100))
                    .toArray(new Array[Status](0))
                    .map {
                        x => new Tweet(x.getText, x.getId,
                            x.getLang, user.getScreenName)
                    }
            }

            val account = new TwitterAccount(handle, id, name, language, url, location, description, tweets)

            return account
        }
        else
            return null
    }
}
