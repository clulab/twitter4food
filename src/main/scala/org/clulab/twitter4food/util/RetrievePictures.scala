package org.clulab.twitter4food.util

import com.typesafe.config.ConfigFactory
import org.clulab.twitter4food.twitter4j.TwitterAPI
import org.slf4j.LoggerFactory

import sys.process._
import java.net.URL
import java.io.File

object RetrievePictures {
  val logger = LoggerFactory.getLogger(this.getClass)
  val config = ConfigFactory.load

  def main(args: Array[String]): Unit = {
    val ids = FileUtils.load(config.getString("classifiers.overweight.data"))
      .toSeq
      .filter(_._1.tweets.nonEmpty)
      .map(_._1.id)

    val numProcesses = 16
    val chunkSize = ids.length / numProcesses

    val base = config.getString("classifiers.overweight.profilePictures")
    val pattern = """(.*)[.]([^.]*)""".r

    for {
      thread <- (0 until numProcesses).par
      api = new TwitterAPI(thread)
    } yield {
      val threadPics = scala.collection.mutable.Map[Long, String]()
      for (i <- thread * chunkSize until (thread + 1) * chunkSize) {
        logger.debug(s"fetching ${ids(i)}")
        val fetched = api.fetchProfilePic(ids(i))
        if(fetched.nonEmpty) {
          val id = ids(i)
          val url = fetched.get
          Thread.sleep(250 + scala.util.Random.nextInt(500)) // Sleep to avoid spamming the server
          val extension = url match { case pattern(fn, ext) => ext }
          try {
            new URL(url) #> new File(base + id + "." + extension) !!
          } catch {
            case e: Exception => logger.error(s"$url not found for user $id")
          }
        }
      }
      threadPics.toMap
    }

    logger.info("Profile pictures retrieved")
  }
}