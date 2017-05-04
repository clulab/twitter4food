package org.clulab.twitter4food.data

import java.io.{BufferedWriter, File, FileWriter}

import com.typesafe.config.{Config, ConfigFactory}
import org.clulab.twitter4food.twitter4j.TwitterAPI
import org.clulab.twitter4food.util.FileUtils
import org.slf4j.{Logger, LoggerFactory}

/**
  * Searches through existing accounts for geotagged tweets and prints out the user id, tweet id, latitude, and
  * longitude, one line per tweet. Takes a long time per account, but < 1% of tweets are geotagged.
  */
class TweetLocs extends App {
  def retrieveCoords(ids: Seq[Long]): Map[Long, Seq[(Long, Double, Double)]] = {
    val numProcesses = 18
    val chunkSize = ids.length / numProcesses

    val pics = for {
      thread <- (0 until numProcesses).par
      api = new TwitterAPI(thread)
      i <- thread * chunkSize until (thread + 1) * chunkSize
    } yield {
      logger.debug(s"fetching ${ids(i)}")
      ids(i) -> api.fetchCoords(ids(i))
    }

    pics.seq.toMap
  }

  val config: Config = ConfigFactory.load
  val logger: Logger = LoggerFactory.getLogger(this.getClass)

  val locFile = new File(config.getString("classifiers.overweight.tweetCoords"))
  val locWriter = new BufferedWriter(new FileWriter(locFile, true))

  // read account IDs in (all else is thrown away, not very efficient)
  val accts = FileUtils.load(config.getString("classifiers.overweight.data"))
    .keys
    .filter(_.tweets.nonEmpty)
    .map(_.id)
    .toSeq

  // get the latitude and longitude using the Twitter API (takes a long time b/c API limits)
  val coords = retrieveCoords(accts)

  // write URLs to file to download later
  for {
    (id, records) <- coords
    (twid, lat, lon) <- records
  } {
    locWriter.write(s"$id,$twid,$lat,$lon\n")
  }

  locWriter.close()

}