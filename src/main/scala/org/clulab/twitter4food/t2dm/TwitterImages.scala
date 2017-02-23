package org.clulab.twitter4food.t2dm

import java.io.{BufferedReader, File, InputStreamReader}
import java.net.URL

import org.apache.commons.io.FilenameUtils
import com.typesafe.config.{Config, ConfigFactory}
import jline.console.ConsoleReader
import org.slf4j.{Logger, LoggerFactory}

import sys.process._
import scala.util.Try

object TwitterImages {

  val config: Config = ConfigFactory.load
  val logger: Logger = LoggerFactory.getLogger(this.getClass)
  // Twitter's robots.txt asks for 1 s between requests
  val twitterDelay = 1010
  // Instagram doesn't list anything, but let's arbitrarily halve Twitter's because we have to make two calls
  val instDelay = 505

  def main(args: Array[String]): Unit = {
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
      case Some("externalImageURLs") => config.getString(s"classifiers.overweight.externalImages")
      case other => config.getString(s"classifiers.overweight.twitterImages") // should never happen
    }

    val inDir = new File(config.getString(s"classifiers.overweight.${site.get}"))
    val users = inDir.list
    if (users.isEmpty) {
      println("No users found. Run TwitterImageURLs to scrape URLs to download.")
      return
    }

    var numToTake: Option[Int] = None
    printNumber(users.length)
    while (numToTake.isEmpty || numToTake.getOrElse(0) < 1) {
      reader.readLine match {
        case valid if Try {
          valid.toInt
        }.isSuccess => numToTake = Option(valid.toInt)
        case other => printNumber(users.length)
      }
    }

    site match {
      case Some("twitterImageURLs") => downloadFromTwitter(users, numToTake.get, inDir.getPath, outDir)
      case Some("externalImageURLs") => downloadFromInstagram(users, numToTake.get, inDir.getPath, outDir)
      case other => ()
    }
  }

  def printSites(): Unit = {
    println("[t] Twitter")
    println("[i] Instagram")
  }

  def printNumber(numAccounts: Int): Unit = {
    println("Enter number of photos to download per account")
    println(s"(Each photo will add ${numAccounts * twitterDelay / 1000.0 / 60.0 / 60.0} hours)")
  }

  def downloadFromTwitter(users: Array[String], numToTake: Int, inDir: String, outDir: String): Unit = {
    val pb = new me.tongfei.progressbar.ProgressBar("downloading", 100)
    pb.start()
    pb.maxHint(users.length)

    // Go through each user's files and try to download numToTake
    // This is intentionally not parallel to avoid spamming the server and getting blacklisted
    users.foreach { userFilename =>
      val id = FilenameUtils.getBaseName(userFilename)
      val userDirName = s"$outDir/$id"
      val userDir = new File(userDirName)
      if (!userDir.exists()) userDir.mkdir

      val previouslyScraped = userDir.list.map(FilenameUtils.getName)

      val photoURLs = scala.io.Source.fromFile(s"$inDir/$userFilename")
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

        val didItWork = Try(url.#>(new File(photoLoc)).!!) // the system command to download
        Thread.sleep(twitterDelay)

        // if at first we don't succeed, try try again just once in case the server was just overloaded
        if (didItWork.isFailure) {
          Try(url.#>(new File(photoLoc)).!!)
          Thread.sleep(twitterDelay)
        }
      }
      pb.step()
    }
    pb.stop()
  }

  def downloadFromInstagram(users: Array[String], numToTake: Int, inDir: String, outDir: String): Unit = {
    val pb = new me.tongfei.progressbar.ProgressBar("downloading", 100)
    pb.start()
    pb.maxHint(users.length)

    // Go through each user's files and try to download numToTake
    // This is intentionally not parallel to avoid spamming the server and getting blacklisted
    users.foreach { userFilename =>
      val id = FilenameUtils.getBaseName(userFilename)
      val userDirName = s"$outDir/$id"
      val userDir = new File(userDirName)
      if (!userDir.exists()) userDir.mkdir

      val previouslyScraped = userDir.list.map(FilenameUtils.getName)

      val urlSegment = "[a-zA-Z0-9_-]+(?=/$)".r

      val pageURLs = scala.io.Source.fromFile(s"$inDir/$userFilename")
        .getLines
        .toSeq
        .flatMap(u => urlSegment.findFirstIn(u))

      var numSoFar = 0
      var i = 0
      while (i < pageURLs.length && numSoFar <= numToTake) {
        val photoURL = convertToImage(pageURLs(i))
        i += 1
        if (photoURL.nonEmpty) {
          val url = new URL(photoURL.get)
          val photoFilename = FilenameUtils.getName(url.getPath)
          val photoLoc = s"$userDir/$photoFilename"

          val didItWork = Try(url.#>(new File(photoLoc)).!!) // the system command to download
          Thread.sleep(instDelay)

          // if at first we don't succeed, try try again just once in case the server was just overloaded
          if (didItWork.isFailure) {
            val secondChance = Try(url.#>(new File(photoLoc)).!!)
            Thread.sleep(instDelay)
            if (secondChance.isSuccess) numSoFar += 1
          } else numSoFar += 1
        }
      }
    }
  }

  // Rather than using Instagram API, load the webpage and find the main photo
  def convertToImage(u: String): Option[String] = {
    val pageUrl = Try{ new URL(u) }
    if (pageUrl.isFailure) return None

    val page = new BufferedReader(new InputStreamReader(pageUrl.get.openStream())).lines.toArray.mkString("\n")
    Thread.sleep(instDelay)

    // This might change in the future
    val pattern = "(?<=<meta property=\"og:image\" content=\")([^\"]+)(?=\" />)".r
    pattern.findFirstIn(page)
  }
}