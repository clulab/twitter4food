package org.clulab.twitter4food.util

import java.io._
import java.text.SimpleDateFormat
import java.util.Date
import org.clulab.twitter4food.struct._
import org.clulab.twitter4food.twitter4j._
import scala.collection.mutable.{ArrayBuffer, Map}

object FileUtils {
    def saveToFile(users: Seq[TwitterAccount], labels: Seq[String],
                   fileName: String) = {
        val writer = new BufferedWriter(new FileWriter(new File(fileName)))
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

    def load(fileName: String) = {
        val numLines = scala.io.Source.fromFile(fileName).getLines.length
        val lines = scala.io.Source.fromFile(fileName).getLines

        /* Lazy declarations */
        var i = 1
        var count = 0
        var handle, name, label, id = ""
        var desc, lang, location, url = ""
        var numTweets = 0
        val accounts = Map[TwitterAccount, String]()
        val pb = new me.tongfei.progressbar.ProgressBar("FileUtils", 100)
        pb.start()
        pb.maxHint(lines.next.toInt)
        pb.setExtraMessage("Loading...")

        while (i < numLines) {
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
                            url, location, desc, tweets) -> label)
                        count += 1
                        pb.step()
                    }
                case 4 =>
                    val tweets = ArrayBuffer[Tweet]()
                    if (numTweets > 0) {
                        var jCount = 0;
                        var j = 0
                        var tweetLines = new Array[String](2 * numTweets)
                        tweetLines(0) = line
                        for (k <- 1 until 2 * numTweets) {
                            tweetLines(k) = lines.next
                        }

                        var tweetId, tweetLang = ""
                        var date: Date = null
                        while (j < tweetLines.length) {
                            val tweetLine = tweetLines(j)
                            val tweetSplit = tweetLine.split("\t")
                            jCount match {
                                case 0 => tweetId = tweetSplit(0)
                                    val df = new SimpleDateFormat("EEE MMM dd HH:mm:ss zzz yyyy")
                                    date = df.parse(tweetSplit(1))
                                    tweetLang = tweetSplit(2)
                                case 1 => tweets += new Tweet(tweetLine, tweetId.toLong,
                                    tweetLang, date, handle)
                            }
                            jCount += 1;
                            jCount %= 2;
                            j += 1
                        }

                        i = i + (2 * numTweets) - 1
                    }

                    accounts += (new TwitterAccount(handle, id.toLong, name, lang,
                        url, location, desc, tweets) -> label)
                    pb.step()
            }
            count += 1;
            count %= 5;
            i += 1
        }
        pb.stop()
        accounts
    }
}