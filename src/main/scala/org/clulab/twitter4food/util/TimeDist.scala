package org.clulab.twitter4food.util

import java.io.{BufferedWriter, File, FileWriter}
import java.time.LocalDateTime

import com.typesafe.config.ConfigFactory

import scala.util.Try

object TimeDist {
  def main(args: Array[String]): Unit = {
    if(args.length != 1) {
      println("Usage: enter a file name to output a csv of time distibution figures")
      return
    }

    val file = Try(new File(args.head)).toOption
    if (file.isEmpty) {
      println(s"Couldn't write to ${args.head}")
      return
    }

    val config = ConfigFactory.load

    val accts = FileUtils.load(config.getString("classifiers.overweight.data"))
      .keys
      .toSeq
      .filter(_.tweets.nonEmpty)

    val zid = java.time.ZoneId.of("GMT")
    val window = 0.1

    def minus(a: LocalDateTime, b: LocalDateTime): Double = {
      val years = (a.getYear - b.getYear) * 365.242199
      val days = (a.getDayOfYear - b.getDayOfYear).toDouble
      val hours = (a.getHour - b.getHour) / 24.0
      val minutes = (a.getMinute - b.getMinute) / 1440.0
      years + days + hours + minutes
    }

    val rows = for {
      portion <- 0.0 until 0.9 by 0.05
      acct <- accts
      allDates = acct.tweets.map(t => LocalDateTime.ofInstant(t.createdAt.toInstant, zid))
      if allDates.nonEmpty
      newest = allDates.sortWith(_.compareTo(_) > 0).head

      start = (portion * acct.tweets.length).toInt
      end = ((portion + window) * acct.tweets.length).toInt
      tweets = allDates.slice(start, end)
      diffs = tweets.map(t => minus(newest, t))
      if diffs.nonEmpty
    } yield {
      val mean = diffs.sum / diffs.length.toDouble
      val mn = diffs.min
      val mx = diffs.max
      f"$portion%1.2f,${Utils.sanitizeHandle(acct.handle)},$mean%1.2f,$mn%1.2f,$mx%1.2f\n"
    }

    val bw = new BufferedWriter(new FileWriter(file.get))
    bw.write("portion,handle,daysMean,daysMin,daysMax\n")
    rows.foreach(row => bw.write(row))
    bw.close()
  }
}