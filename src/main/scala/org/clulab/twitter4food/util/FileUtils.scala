package org.clulab.twitter4food.util

import java.io._
import java.text.SimpleDateFormat
import java.util.Date

import org.clulab.twitter4food.struct._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Try

object FileUtils {
  def normalizeText(text: String): String = text.replaceAll("[\0\b\t\n\f\r]", " ").replaceAll(" +", " ")

  def encapsulate(texts: Seq[String]): String = if (texts.isEmpty) "" else texts.mkString("|||")

  def expose(text: String): Seq[String] = if (text == "") Nil else text.split("\\|\\|\\|")

  def saveToFile(users: Seq[TwitterAccount], labels: Seq[String],
    fileName: String, append: Boolean = false) = {
    val writer = new BufferedWriter(new FileWriter(fileName, append))
    assert(labels.size == users.size)

    val numValidAccounts = users.foldLeft(0)((s, u) => if (u != null) s + 1 else s)
    writer.write(s"${numValidAccounts.toInt}\n")

    for (i <- labels.indices) {
      val user = users(i)
      try {
        if (user != null) {
          writer.write(s"${user.handle}\t${labels(i)}\n")
          writer.write(s"${user.handle}\t${user.id}\t")
          writer.write(s"${normalizeText(user.name)}\t")
          writer.write(s"${user.tweets.size}\n")
          writer.write(s"${user.lang}\t${normalizeText(user.url)}\t")
          writer.write(s"${normalizeText(user.location)}\n")
          writer.write(s"${normalizeText(user.description)}\n")
          user.tweets.foreach { tweet =>
            writer.write(s"${tweet.id}\t${tweet.createdAt}\t${tweet.lang}\t")
            writer.write(s"${encapsulate(tweet.urls)}\t${encapsulate(tweet.images)}\t${encapsulate(tweet.extImages)}\n")
            writer.write(s"${normalizeText(tweet.text)}\n")
          }
        }
      } catch {
        case e: IOException => e.printStackTrace()
      }
    }
    writer.close()
  }

  def loadTwitterAccounts(fileName: String) = {
    val file = scala.io.Source.fromFile(fileName)
    val lines = file.getLines

    /* Lazy declarations */
    var count = 0
    var handle, name, label, id = ""
    var desc, lang, location, url = ""
    var numTweets = 0
    val accounts = mutable.Map[TwitterAccount, String]()
    val pb = new me.tongfei.progressbar.ProgressBar("FileUtils", 100)
    pb.start()
    if(lines.hasNext) {
      pb.maxHint(lines.next.toInt)
      pb.setExtraMessage("Loading...")
    }

    while (lines.hasNext) {
      val line = lines.next
      //println(s"$count, $line")
      val splits = line.split("\t")
      count match {
        case 0 => label = splits(1)
        case 1 => handle = splits(0)
          id = splits(1)
          name = splits(2)
          numTweets = splits(3).toInt
        case 2 => lang = splits(0)
          if (splits.length > 1) url = splits(1)
          if (splits.length > 2) location = splits(2)
        case 3 => desc = line
          if (numTweets == 0) {
            val tweets = ArrayBuffer[Tweet]()
            accounts += (new TwitterAccount(handle, id.toLong, name, lang,
              url, location, desc, tweets, Seq[TwitterAccount]()) -> label)
            count += 1
            pb.step()
          }
        case 4 =>
          val tweets = ArrayBuffer[Tweet]()
          if (numTweets > 0) {
            var jCount = 0
            val tweetLines = (line :: lines.slice(0, 2*numTweets - 1).toList).iterator

            var tweetId, tweetLang = ""
            var date: Date = null
            var urls: Seq[String] = Nil
            var images: Seq[String] = Nil
            var extImages: Seq[String] = Nil
            while (tweetLines.hasNext) {
              val tweetLine = tweetLines.next
              val tweetSplit = tweetLine.split("\t")
              jCount match {
                case 0 => tweetId = tweetSplit(0)
                  val df = new SimpleDateFormat("EEE MMM dd HH:mm:ss zzz yyyy")
                  date = df.parse(tweetSplit(1))
                  tweetLang = tweetSplit(2)
                  urls = if (tweetSplit.length < 4) Nil else tweetSplit(3).split("\\|\\|\\|")
                  images = if (tweetSplit.length < 5) Nil else tweetSplit(4).split("\\|\\|\\|")
                  extImages = if (tweetSplit.length < 6) Nil else tweetSplit(5).split("\\|\\|\\|")
                case 1 => tweets += new Tweet(tweetLine, tweetId.toLong,
                  tweetLang, date, handle, urls, images, extImages)
              }
              jCount += 1
              jCount %= 2
            }
          }

          accounts += (new TwitterAccount(handle, id.toLong, name, lang,
            url, location, desc, tweets, Seq[TwitterAccount]()) -> label)
          pb.step()
      }
      count += 1
      count %= 5
    }
    file.close
    pb.stop()
    accounts.toMap
  }

  def loadTwoLineTexts(fileName: String, englishOnly: Boolean = true): Seq[String] = {
    val file = scala.io.Source.fromFile(fileName)
    val lines = file.getLines

    val texts = new ArrayBuffer[String]()

    /* Lazy declarations */
    var count = 0
    var numTweets = 0
    var lang = ""

    // Two-line files have a line at the top saying how many accounts are in it
    val pb = new me.tongfei.progressbar.ProgressBar("FileUtils", 100)
    pb.start()
    if(lines.hasNext) {
      pb.maxHint(lines.next.toInt)
      pb.setExtraMessage("Loading...")
    }

    while (lines.hasNext) {
      count match {
        case 1 =>
          val line = lines.next
          val splits = line.split("\t")
          numTweets = try {
            splits(3).toInt // number of tweets for this account
          } catch {
            case e: IndexOutOfBoundsException => println(line)
              splits.last.toInt
          }
        case 2 =>
          val line = lines.next
          val splits = line.split("\t")
          lang = splits(0) // must be "en" if englishOnly
        case 4 => // tweets start here
          if (numTweets > 0) {
            for {
              j <- 0 until numTweets
              _ = lines.next // ignore tweet metadata
              tweetLine = lines.next
              if !englishOnly || lang == "en"
            } texts.append(tweetLine.stripLineEnd)
            pb.step()
          }
        case other => val line = lines.next // just skip it
      }

      count += 1
      count %= 5
    }

    pb.stop()

    file.close

    texts
  }

  def loadThreeLineTexts(fileName: String, englishOnly: Boolean = true): Seq[String] = {
    val fileInit = scala.io.Source.fromFile(fileName)
    val hint = fileInit.getLines.length / 3
    fileInit.close

    val file = scala.io.Source.fromFile(fileName)
    val lines = file.getLines

    val texts = new ArrayBuffer[String]()

    var count = 0
    var lang = ""
    val pb = new me.tongfei.progressbar.ProgressBar("FileUtils", 100)
    pb.start()
    pb.maxHint(hint)
    pb.setExtraMessage("Loading...")

    lines.foreach { line =>
      count match {
        case 0 => lang = line.stripLineEnd.split("\t").last
        case 2 =>
          if (lang == "en") texts.append(line.stripLineEnd)
          pb.step()
        case other => () // do nothing
      }
      count += 1
      count %= 3
    }
    pb.stop()

    file.close

    texts
  }

  def writeToCsv(fileName: String, toWrite: Seq[Seq[String]], sep: String = ","): Unit = {
    val writer = new BufferedWriter(new FileWriter(fileName, false))
    toWrite.foreach{ row => writer.write(row.mkString(sep) + "\n") }
    writer.close()
  }

  def readFromCsv(fileName: String, sep: String = ","): Seq[Seq[String]] = {
    val file = scala.io.Source.fromFile(fileName)
    file.getLines.map(_.trim.split(sep).toSeq).toSeq
  }

  /**
    * Returns a [[Seq]] of [[Location]]s from a file standoff as produced by [[saveLocations()]]
    */
  def loadLocations(fileName: String): Seq[Location] = {
    val df = new SimpleDateFormat("EEE MMM dd HH:mm:ss zzz yyyy")

    if (!new File(fileName).exists) return Nil

    val hint = scala.io.Source.fromFile(fileName).getLines.toSeq.length

    val file = scala.io.Source.fromFile(fileName)
    val lines = file.getLines

    val pb = new me.tongfei.progressbar.ProgressBar("FileUtils", 100)
    pb.start()
    if(lines.hasNext) {
      pb.maxHint(hint)
      pb.setExtraMessage("Loading...")
    }

    val locs = for (
      line <- lines
    ) yield {
      val locData = line.stripLineEnd.split("\t")
      val id = locData(0)
      val lat = locData(1).toDouble
      val lng = locData(2).toDouble
      val user = locData(3).toLong
      val createdAt = df.parse(locData(4))
      val source = locData(5)
      val venueText = if(locData.length > 6) locData(6) else ""
      val venues: Try[Seq[Venue]] = Try {
        if (venueText.isEmpty) Nil else {
          venueText
            .drop(1)
            .dropRight(1)
            .split("\\), \\(")
            .map { v =>
              val elements = v.split(";")
              val name = elements(0)
              val types = elements(1)
                .drop(1)
                .dropRight(1)
                .split(":")
              val lat = elements(2).toDouble
              val lng = elements(3).toDouble
              new Venue(name, types, lat, lng)
            }
        }
      }
      if (venues.isFailure) {
        println(s"Failed to parse venues from:\n$line")
        System.exit(1)
      }
      pb.step()
      new Location(id, lat, lng, user, createdAt, source, venues.get)

    }
    pb.stop()

    locs.toSeq
  }

  /**
    * Saves a [[Seq]] of [[Location]]s to a file standoff loadable by [[loadLocations()]]
    */
  def saveLocations(locs: Seq[Location], fileName: String, append: Boolean = false) = {
    val writer = new BufferedWriter(new FileWriter(fileName, append))

    locs.foreach(loc => writer.write(s"${loc.toString}\n"))

    writer.close()
  }
}