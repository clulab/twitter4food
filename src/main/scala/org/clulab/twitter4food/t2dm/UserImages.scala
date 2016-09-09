package org.clulab.twitter4food.t2dm

import java.io.{BufferedWriter, File, FileWriter}

import com.typesafe.config.ConfigFactory
import org.clulab.twitter4food.twitter4j.TwitterAPI

import scala.util.Random

object UserImages {

  def retrievePics(names: Seq[String], n: Int): Map[String, String] = {
    val numProcesses = 16
    val chunkSize = names.length / numProcesses
    val goalSize = n / numProcesses + n % numProcesses

    val pics = for {
      thread <- (0 until numProcesses).par
      api = new TwitterAPI(thread)
    } yield {
      val threadPics = scala.collection.mutable.Map[String, String]()
      var i = thread * chunkSize
      while (i < (thread + 1) * chunkSize & threadPics.size < goalSize) {
        val fetched = api.fetchProfilePic(names(i))
        if(fetched.nonEmpty) threadPics += names(i) -> fetched.get
        i += 1
      }
      threadPics.toMap
    }

    pics.seq.flatten.toMap
  }

  /**
    * Get the profile picture URLs for n usernames, where n is the sole input to this function
    *
    * @param args
    */
  def main (args: Array[String]): Unit = {
    assert(args.length >= 1, "Usage: enter number of profiles to retrieve profile pictures for")

    val config = ConfigFactory.load
    val alreadyCollected = scala.io.Source.fromFile(config.getString("classifiers.overweight.handles"))
      .getLines()
      .map(_.split("\t").head)
      .toSet
    val available = scala.io.Source.fromFile(config.getString("classifiers.overweight.midrange"))
      .getLines()
      .map(_.split("\t").head)
      .toSet

    val seed = 37
    val r = new Random(seed)
    val toChooseFrom = r.shuffle(available.diff(alreadyCollected).toSeq)

    val found = retrievePics(toChooseFrom, args.head.toInt)

    val writer = new BufferedWriter(new FileWriter(new File(config.getString("classifiers.overweight.handles")), true))

    found.foreach{case (handle, url) => writer.write(s"handle\turl\n")}

    writer.close()
  }
}