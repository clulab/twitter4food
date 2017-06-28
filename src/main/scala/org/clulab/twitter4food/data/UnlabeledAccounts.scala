package org.clulab.twitter4food.data

import com.typesafe.config.ConfigFactory
import org.clulab.twitter4food.util.FileUtils
import org.slf4j.LoggerFactory
import org.clulab.struct.Counter
import org.clulab.twitter4food.twitter4j.TwitterAPI

import scala.collection.mutable
import sys.process._

object UnlabeledAccounts extends App {
  val logger = LoggerFactory.getLogger(this.getClass)
  val config = ConfigFactory.load

  // we want accounts within this range of occurrences in the food corpus, to match the overweight corpus collection
  val minAppearances = 10
  val maxAppearances = 7000

  // exact organization names to exclude, e.g. 'Baskin-Robbins'
  val orgNames = scala.io.Source.fromFile(config.getString("classifiers.human.orgNames"))
    .getLines
    .map(_.toLowerCase)
    .toSet
  // organization keywords, e.g. 'grille'
  val orgWords = scala.io.Source.fromFile(config.getString("classifiers.human.orgWords"))
    .getLines
    .map(_.toLowerCase)
    .toSet

  // the API that will collect user data
  // this keyset (19) reserved for this purpose.
  val api = new TwitterAPI(19)

  logger.info("Loading previously gathered unlabeled Twitter accounts")
  val unlabledLoc = config.getString("classifiers.unlabeled.tweets")
  val unlabeledAccounts = FileUtils.loadTwitterAccounts(unlabledLoc)
    .map{ case (acct, lbl) => acct.id }
    .toSet

  logger.info("Loading already-labeled Twitter accounts")
  val labeledAccts = FileUtils.loadTwitterAccounts(config.getString("classifiers.overweight.data"))
    .map{ case (acct, lbl) => acct.id }
    .toSet

  // to exclude already-collected accounts
  val previouslySeen = unlabeledAccounts ++ labeledAccts

  // source of candidate IDs, updated live
  val foodTweetFile = config.getString("food_tweets")
  // copy of source, for easy access
  val foodTweetCopy = config.getString("food_tweet_copy")

  // for checking whether enough time has elapsed to check food tweets for more IDs
  var refTime = System.currentTimeMillis()

  // all IDs processed during this run, suitable or not
  val seenSoFar = mutable.ArrayBuffer[Long]()

  while (true) {
    val appearances = countAppearances()

    appearances.foreach { case (id, count) =>
      logger.info(s"Retrieving $id")

      seenSoFar.append(id)

      if (appropriate(id)) {
        // gather acct information -- for now, just the tweets
        val acct = api.fetchAccount(id.toString, fetchTweets = true, isID = true)
        FileUtils.saveToFile(Seq(acct), Seq("unknown"), unlabledLoc, append = true)
      }
    }
  }

  // returns a list of the accounts that haven't been collected, sorted by number of appearances
  def countAppearances(lastRun: Long = 0): Seq[(Long, Double)] = {
    val sinceLastCount = System.currentTimeMillis() - lastRun

    // if < 24 hrs since last copy/backup, wait
    if (sinceLastCount < (24 * 60 * 60 * 1000)) Thread.sleep(sinceLastCount)
    refTime = System.currentTimeMillis()

    // find the IDs that occur in the food tweets
    logger.info("(Re)loading food tweets")
    s"cp $foodTweetFile $foodTweetCopy".!!
    val ids = FileUtils.loadThreeLineIds(foodTweetCopy)

    // count occurrences of IDs in food collection
    val c = new Counter[Long]
    ids.foreach(id => c.incrementCount(id))

    // ignore IDs already in annotated collection or that appear too often or too seldom
    val selected = c.filter{ case (id, count) =>
      count <= maxAppearances &&
        count >= minAppearances &&
        ! previouslySeen.contains(id)
    }

    selected.sorted(descending = true)
  }

  def appropriate(id: Long): Boolean = {
    val user = api.appOnlyTwitter.showUser(id)
    // if user is nonexistent or protected, we can't get enough info
    if (user == null || user.getStatus == null) {
      logger.info(s"Failure: $id protected or nonexistent")
      return false
    }
    // English only for compatibility with model
    if (user.getLang != "en") {
      logger.info(s"Failure: $id uses non-English language primarily")
      return false
    }
    val name = user.getName
    // user must not match an org name exactly
    if (orgNames contains name) {
      logger.info(s"Failure: $id's name $name is an organization name")
      return false
    }
    // user must not have organization words in name, e.g. 'Thai Kitchen'
    val nameWords = name.split("\\s+").map(_.toLowerCase).toSet
    if (nameWords.intersect(orgWords).nonEmpty) {
      logger.info(s"Failure: $id's name $name contains organization keywords")
      return false
    }
    // user name must not be possessive, e.g. 'Charley's'
    if (nameWords.exists(w => w.matches("('s|s')$"))) {
      logger.info(s"Failure: $id's name $name contains a possessive")
      return false
    }

    logger.info(s"Success: $id with name $name seems human")
    true
  }
}