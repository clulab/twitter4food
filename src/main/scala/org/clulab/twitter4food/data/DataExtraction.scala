package org.clulab.twitter4food.data

import java.io.{BufferedWriter, File, FileWriter}
import java.net.URL
import java.nio.file.{Files, Paths}

import sys.process._
import com.typesafe.config.ConfigFactory
import org.apache.commons.io.FilenameUtils
import org.clulab.twitter4food.data.ProfileImagesFromHandles.retrieveProfilePics
import org.slf4j.LoggerFactory
import org.clulab.twitter4food.util.FileUtils
import org.clulab.twitter4food.util.Utils.sanitizeHandle
import org.clulab.twitter4food.struct.TwitterAccount
import org.clulab.twitter4food.twitter4j.TwitterAPI

import scala.util.Try

case class DataExtractionConfig(variable: String = "diabetes",
                                getImages: Boolean = false,
                                twitterImages: Boolean = true,
                                extImages: Boolean = true)

/**
  * Author: Dane Bell
  *
  * A script for downloading the tweets of a file of twitter handles.
  * The file is assumed to have the format of
  * [handle]\t[classificationLabel]
  * on each line.
  */
object DataExtraction {
  // Twitter's robots.txt asks for 1 s between requests
  val twitterDelay = 1010
  // Instagram doesn't list anything, so let's choose arbitrarily
  val instDelay = 100

  val logger = LoggerFactory.getLogger(this.getClass)

  def parseArgs(args: Array[String]): DataExtractionConfig = {
    val parser = new scopt.OptionParser[DataExtractionConfig]("dataExtraction") {
      head("Choose a variable from the set {overweight, ow2, diabetes, human, gender}")
      arg[String]("variable") action { (x, c) => c.copy(variable = x) } text "variable to use"
      opt[String]('p', "photos") action { (x, c) => c.copy(getImages = true) } text "download photos if true"
      opt[String]('t', "twitter") action { (x, c) => c.copy(twitterImages = true) } text "twitter images if true"
      opt[String]('i', "instagram") action { (x, c) => c.copy(extImages = true) } text "instagram images if true"
    }

    val opts = parser.parse(args, DataExtractionConfig())

    if(opts.isEmpty) throw new IllegalArgumentException(s"args ${args.mkString(" ")} are not supported!")

    opts.get
  }

  def retrieveAccounts[T](names: Seq[T], numProcesses: Int = 16): Seq[TwitterAccount] = {
    assert(names.isEmpty ||
      names.head.isInstanceOf[String] ||
      names.head.isInstanceOf[Long],
      "TwitterAccount identifiers must be Strings or Longs!")

    val chunkSize = names.length / numProcesses

    var steps = 0

    val accounts = for {
      thread <- (0 until numProcesses).par
      api = new TwitterAPI(thread)
      startIdx = thread * chunkSize
      lastIdx = if (thread + 1 == numProcesses) names.length else (thread + 1) * chunkSize
      i <- (startIdx until lastIdx).seq
    } yield {
      steps += 1
      println(s"$steps/${names.length} ${names(i)}")
      val fetched = names(i) match {
        case handle: String => api.fetchAccount(handle, fetchTweets = true)
        case id: Long => api.fetchAccount(id.toString, fetchTweets = true, isID = true)
      }
      fetched
    }

    accounts.seq
  }

  def getImages(users: Seq[TwitterAccount], outDir: String,
                getTwitter: Boolean = false, getExt: Boolean = false, maxDownloads: Int = 1000) {

    val message = if (getTwitter && getExt) "All images"
    else if (getTwitter)
      "Twitter images"
    else if (getExt)
      "External images"
    else
      ""

    val pb = new me.tongfei.progressbar.ProgressBar(message, 100)
    pb.start()
    pb.maxHint(users.length)

    // Go through each user's files and try to download numToTake
    // This is intentionally not parallel to avoid spamming the server and getting blacklisted
    users.foreach { user =>
      val id = user.id
      val userDirName = s"$outDir/$id"
      val userDir = new File(userDirName)
      if (!userDir.exists()) userDir.mkdir

      val previouslyScraped = userDir
        .list
        .map(FilenameUtils.getBaseName)
        .map(_.split("-").head)

      // keep track of how many downloaded so we don't exceed max allowed
      var totalDownloaded = previouslyScraped.length

      // to exclude previously downloaded images
      val previousTweets = previouslyScraped.toSet

      // Ignore RTs when downloading images
      val tweetsOfInterest = user.tweets.filter(!_.isRetweet)

      // index of tweet among tweets
      var i = 0
      while (totalDownloaded <= maxDownloads && i < tweetsOfInterest.length) {
        val tweet = tweetsOfInterest(i)
        if(!previousTweets.contains(tweet.id.toString)) {
          var j = 0
          val twUrls = if (getTwitter) tweet.images else Nil
          val extUrls = if (getExt) tweet.extImages else Nil

          (twUrls ++ extUrls).foreach { u =>
            val url = new URL(u)
            val extension = FilenameUtils.getExtension(url.getPath)
            // we rename the image, which is dangerous but important for aligning tweets to images
            val photoLoc = s"$userDir/${tweet.id}-$j.$extension"

            val didItWork = Try(url.#>(new File(photoLoc)).!!) // the system command to download
            Thread.sleep(twitterDelay)

            // if at first we don't succeed, try try again just once in case the server was just overloaded
            val overallSuccess = if (didItWork.isFailure) {
              val secondTry = Try(url.#>(new File(photoLoc)).!!)
              Thread.sleep(twitterDelay)
              secondTry.isSuccess
            } else true

            if (overallSuccess) {
              j = j + 1
              totalDownloaded = totalDownloaded + 1
            }
          }
        }
        i = i + 1
      }
      pb.step()
    }
    pb.stop()
  }

  def noDuplicates(fn: String): String = {
    if (!Files.exists(Paths.get(fn))) return fn
    var i = 1
    var ret = fn + i
    while(Files.exists(Paths.get(ret))) {
      i = i + 1
      ret = fn + i
    }
    ret
  }

  def main(args: Array[String]) {
    val opts = parseArgs(args)

    logger.info(s"Updating tweets for ${opts.variable} classification")

    val config = ConfigFactory.load
    val outputFile = config.getString(s"classifiers.${opts.variable}.data_raw")
    val inputFileStr = config.getString(s"classifiers.${opts.variable}.handles")

    // Save a copy of the existing accounts in case something goes wrong
    val corpusExists = Files.exists(Paths.get(outputFile))
    val backupLocation = noDuplicates(s"${outputFile + ".backup"}")
    if (corpusExists) s"cp $outputFile $backupLocation".!
    logger.info(s"Backup of existing file $outputFile saved to $backupLocation")

    // already saved accounts (still update)
    val previous: Seq[(TwitterAccount, String)] = if (corpusExists)
      FileUtils.loadTwitterAccounts(outputFile).toSeq
    else
      Nil
    val (prevAccounts, _) = previous.unzip

    // file containing handles and labels
    val inputFile = scala.io.Source.fromFile(inputFileStr)
    val lines = inputFile.getLines

    val pairs = for (line <- lines) yield {
      // Parse line
      val tuple = line.stripLineEnd.split("\t")
      val handle = sanitizeHandle(tuple.head)
      val label = tuple.last
      (handle, label)
    }

    val labeledHandles = pairs.toMap
    val handles = labeledHandles.keys.toSeq

    inputFile.close()

    // download all labeled accounts, regardless of whether they were loaded before
    val accounts = retrieveAccounts(handles).filterNot(_ == null)
    val labels = accounts.map(acct => labeledHandles.getOrElse(sanitizeHandle(acct.handle), "NULL"))

    // Update existing accounts. Notice that we keep all the account's *new* information (e.g. description) which could
    // have changed. Likewise, we are keeping only the new labels.
    val updated = for (account <- accounts) yield {
      val query = prevAccounts.filter(_.id == account.id)
      if (query.nonEmpty)
        account.merge(query.head)
      else
        account
    }

    // Some old accounts may have
    //  1. changed their handles,
    //  2. been deactivated, or
    //  3. made private,
    // but we still want them. We will check IDs for updates and then just keep the rest as is.
    val updatedIds = updated.map(_.id)
    val lostIds = previous.map(_._1.id).filterNot(id => updatedIds.contains(id))

    val newLostAccounts = retrieveAccounts(lostIds).filterNot(_ == null)
    val (oldLostAccounts, lostLabels) = previous.filterNot{ case (acct, lbl) => updatedIds.contains(acct.id) }.unzip

    // Update existing accounts whose names have not been found. Notice that we prefer to keep all the account's *new*
    // information (e.g. description), which could have changed.
    val updatedLost = for (acct <- oldLostAccounts) yield {
      val query = newLostAccounts.filter(_.id == acct.id)
      if (query.nonEmpty)
        query.head.merge(acct)
      else
        acct
    }

    val allAccounts = updated ++ updatedLost
    val allLabels = labels ++ lostLabels
    val (nonEmptyAccounts, nonEmptyLabels) = allAccounts.zip(allLabels).filter(_._1.tweets.nonEmpty).unzip

    logger.info("Saving text to file...")
    FileUtils.saveToFile(nonEmptyAccounts, nonEmptyLabels, outputFile)

    // download images if user asked for them
    if (opts.getImages) {
      val ids = allAccounts.map(_.id)

      logger.info("Getting profile pics...")

      // file for recording profile picture URLS
      val outf = new File(config.getString(s"classifiers.${opts.variable}.profile_pic_urls"))
      // make directory for twitter images if necessary
      val outDir = config.getString(s"classifiers.${opts.variable}.twitterImages")
      val od = new File(outDir)
      if (!od.exists()) od.mkdir

      // get URLs first
      val profilePicURLs = retrieveProfilePics(ids)

      val pb = new me.tongfei.progressbar.ProgressBar("download profile pics", 100)
      pb.start()
      pb.maxHint(profilePicURLs.size)

      // download the profile images themselves
      // this is intentionally not parallel to avoid spamming the server and getting blacklisted
      profilePicURLs.foreach { case (id, u) =>
        val userDirName = s"$outDir/$id"
        val userDir = new File(userDirName)
        if (!userDir.exists()) userDir.mkdir

        val url = new URL(u)
        val ext = FilenameUtils.getExtension(url.getPath)
        val photoLoc = s"$userDir/profile.$ext"

        val didItWork = Try(url.#>(new File(photoLoc)).!!) // the system command to download
        Thread.sleep(twitterDelay)

        // if at first we don't succeed, try try again just once in case the server was just overloaded
        if (didItWork.isFailure) {
          Try(url.#>(new File(photoLoc)).!!)
          Thread.sleep(twitterDelay)
        }
        pb.step()
      }
      pb.stop()

      // download tweet images (on Twitter or externally (on Instagram) according to options
      if(opts.twitterImages || opts.extImages) logger.info("Getting tweet pics...")
      if(opts.twitterImages)
        getImages(nonEmptyAccounts, s"classifiers.${opts.variable}.twitterImages", getTwitter = true)
      if(opts.extImages)
        getImages(nonEmptyAccounts, s"classifiers.${opts.variable}.extImages", getExt = true)
    }
  }
}
