package org.clulab.twitter4food.util

import java.io._
import java.text.SimpleDateFormat
import java.util.Date
import org.clulab.twitter4food.struct._
import scala.collection.mutable.{ArrayBuffer, Map}

object FileUtils {
  def saveToFile(users: Seq[TwitterAccount], labels: Seq[String],
    fileName: String, append: Boolean = false) = {
    val writer = new BufferedWriter(new FileWriter(fileName, append))
    assert(labels.size == users.size)

    val numValidAccounts = users.foldLeft(0)((s, u) => if (u != null) s + 1 else s)
    writer.write(s"${numValidAccounts.toInt}\n")

    for (i <- labels.indices) {
      val user = users(i)
      val ctrlChars = "[\0\b\t\n\f\r]"
      try {
        if (user != null) {
          writer.write(s"${user.handle}\t${labels(i)}\n")
          writer.write(s"${user.handle}\t${user.id}\t")
          writer.write(s"${user.name.replaceAll(ctrlChars, " ")}\t")
          writer.write(s"${user.tweets.size}\n")
          writer.write(s"${user.lang}\t${user.url.replaceAll(ctrlChars, " ")}\t")
          writer.write(s"${user.location.replaceAll(ctrlChars, " ")}\n")
          writer.write(s"${user.description.replaceAll(ctrlChars, " ")}\n")
          user.tweets.foreach(tweet => {
            writer.write(s"${tweet.id}\t${tweet.createdAt}\t${tweet.lang}\n")
            writer.write(s"${tweet.text.replaceAll(ctrlChars, " ")}\n")
          })
        }
      } catch {
        case e: IOException => e.printStackTrace()
      }
    }
    writer.close()
  }

  def load(fileName: String): scala.collection.mutable.Map[TwitterAccount, String] = {
    val file = scala.io.Source.fromFile(fileName)
    val lines = file.getLines

    /* Lazy declarations */
    var count = 0
    var handle, name, label, id = ""
    var desc, lang, location, url = ""
    var numTweets = 0
    val accounts = scala.collection.mutable.Map[TwitterAccount, String]()
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
            while (tweetLines.hasNext) {
              val tweetLine = tweetLines.next
              val tweetSplit = tweetLine.split("\t")
              jCount match {
                case 0 => tweetId = tweetSplit(0)
                  val df = new SimpleDateFormat("EEE MMM dd HH:mm:ss zzz yyyy")
                  date = df.parse(tweetSplit(1))
                  tweetLang = tweetSplit(2)
                case 1 => tweets += new Tweet(tweetLine, tweetId.toLong,
                  tweetLang, date, handle)
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
    accounts
  }

  def loadSingletonTexts(fileName: String): Seq[String] = {
    val file = scala.io.Source.fromFile(fileName)
    val lines = file.getLines.toList
    file.close

    val texts = new ArrayBuffer[String]()

    var count = 0
    val pb = new me.tongfei.progressbar.ProgressBar("FileUtils", 100)
    pb.start()
    pb.maxHint(lines.length)
    pb.setExtraMessage("Loading...")


    lines.foreach { line =>
      count match {
        case 2 => texts.append(line.stripLineEnd)
        case firstTwo => () // ignore first two lines of each triple
      }
      count += 1
      count %= 3

      pb.step()
    }
    pb.stop()
    
    texts
  }
}