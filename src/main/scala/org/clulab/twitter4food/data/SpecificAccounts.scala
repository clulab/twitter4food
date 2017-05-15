package org.clulab.twitter4food.data

import java.io.File

import com.typesafe.config.{Config, ConfigFactory}
import org.clulab.twitter4food.struct.TwitterAccount
import org.clulab.twitter4food.twitter4j.TwitterAPI
import org.clulab.twitter4food.util.FileUtils.loadTwitterAccounts
import org.slf4j.{Logger, LoggerFactory}

import scala.util.Try

object SpecificAccounts extends App {
  def getAccounts(names: Seq[String]): Map[String, TwitterAccount] = {
    val numProcesses = 16
    val chunkSize = names.length / numProcesses

    val accounts = for {
      thread <- (0 until numProcesses).par
      api = new TwitterAPI(thread)
    } yield {
      val threadAccounts = scala.collection.mutable.Map[String, TwitterAccount]()
      var i = thread * chunkSize
      while (i < (thread + 1) * chunkSize) {
        logger.debug(s"fetching ${names(i)}")
        val fetched = Try(api.fetchAccount(names(i), fetchTweets = true))
        if(fetched.isSuccess) threadAccounts += names(i) -> fetched.get
        i += 1
      }
      threadAccounts.toMap
    }

    accounts.seq.flatten.toMap
  }


  val config: Config = ConfigFactory.load
  val logger: Logger = LoggerFactory.getLogger(this.getClass)

  val desiderata = scala.io.Source.fromFile(config.getString("handpicked_handles")).getLines().toSeq.map(_.stripLineEnd)
  val existingFile = new File(config.getString("handpicked_accounts"))
  val existing = if (existingFile.exists) loadTwitterAccounts(config.getString("handpicked_accounts"))
    .keys
    .filter(_.tweets.nonEmpty)
    .map(_.handle)
    .toSet
  else Set[String]()

  val leftToDo = (desiderata.toSet &~ existing).toSeq

  val accounts = getAccounts(leftToDo)
}