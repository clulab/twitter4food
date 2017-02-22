package org.clulab.twitter4food.t2dm

import java.io.File
import java.net.URL

import org.apache.commons.io.FilenameUtils
import com.typesafe.config.ConfigFactory
import jline.console.ConsoleReader
import org.slf4j.{Logger, LoggerFactory}

import sys.process._
import scala.util.Try

object TwitterImages {

  val logger: Logger = LoggerFactory.getLogger(this.getClass)
  // Twitter's robots.txt asks for 1 s between requests
  val delay = 1010

  def main(args: Array[String]): Unit = {
    val config = ConfigFactory.load

    val reader = new ConsoleReader
    reader.setPrompt(">>> ")

    var site: Option[String] = None
    printSites()
    while (site.isEmpty) {
      reader.readLine match {
        case "t" => site = Option("twitterImageURLs")
        case "i" => site = Option("externalImageURLs")
        case other => printSites()
      }
    }

    val outDir = site match {
      case Some("twitterImageURLs") => config.getString(s"classifiers.overweight.twitterImages")
      case Some("externalImageURLs") => config.getString(s"classifiers.overweight.instagramImages")
      case other => config.getString(s"classifiers.overweight.twitterImages") // should never happen
    }

    val dir = new File(config.getString(s"classifiers.overweight.${site.get}"))
    val users = dir.list
    if (users.isEmpty) {
      println("No users found. Run TwitterImageURLs to scrape URLs to download.")
      return
    }

    var numToTake: Option[Int] = None
    printNumber()
    while (numToTake.isEmpty || numToTake.getOrElse(0) < 1) {
      reader.readLine match {
        case valid if Try{valid.toInt}.isSuccess => numToTake = Option(valid.toInt)
        case other => printNumber()
      }
    }

    val totalTime = users.length * numToTake.get * delay / 1000.0 / 60.0 / 60.0
    logger.info(f"Downloading will take > $totalTime%1.2f hours")

    site match {
      case Some("twitterImageURLs") => downloadFromTwitter(users, numToTake.get, outDir)
      case other => println("Not supported yet!")
    }
  }

  def printSites(): Unit = {
    println("[t] Twitter")
    println("[i] Instagram")
  }

  def printNumber(): Unit = {
    println("Enter number of photos to download per account")
    println(s"${}")
  }

  def downloadFromTwitter(users: Array[String], numToTake: Int, outDir: String): Unit = {
    // Go through each user's files and try to download numToTake
    // This is intentionally not parallel to avoid spamming the server and getting blacklisted
    users.foreach{ userFilename =>
      val id = FilenameUtils.getBaseName(userFilename)
      logger.info(s"retrieving $id")
      val userDir = s"$outDir/$id}"
      val previouslyScraped = new File(userDir).list.map(FilenameUtils.getName)

      val photoURLs = scala.io.Source.fromFile(userFilename)
        .getLines
        .toSeq
        .filter(_.contains("/media/")) // Don't take video thumbs
        .map(_.trim)
        .filterNot(f => previouslyScraped contains FilenameUtils.getName(f)) // ignore already-downloaded
        .distinct // no repeats
        .take(numToTake) // only download numToTake

      photoURLs.foreach { u =>
        val url = new URL(u)
        val photoFilename = FilenameUtils.getName(url.getPath)
        val photoLoc = s"$userDir/$photoFilename"

        val didItWork = Try{url.#>(new File(photoLoc)).!!} // the system command to download
        Thread.sleep(delay)

        // if we failed once, try again just once in case the server was just overloaded
        if(didItWork.isFailure) {
          Try{url.#>(new File(photoLoc)).!!}
          Thread.sleep(delay)
        }
      }
    }
  }
}