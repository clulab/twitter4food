package edu.arizona.sista.twitter4food

import java.io._
import collection.JavaConversions._

object DumpTweets {
  def main(args: Array[String]) {
    batchProcess(args(0))
  }

  def dumpTweets(file: File, tweets : Seq[Tweet], withLocation:Boolean = false) = {
    val pw = new PrintWriter(file)
    for (tweet <- tweets) {
      val str = TwitterPreprocessor.processTokens(tweet.tokens).mkString(" ")
      if (withLocation)
        pw.println(s"${tweet.normalizedLocation}\t$str")
      else
        pw.println(str)
    }
    pw.close
  }

  def batchProcess(queryFile: String) = {
    val pager = new SolrPager
    for (line <- io.Source.fromFile(queryFile).getLines) {
      val tokens = line.split(" ")
      val filename = tokens.head
      val query = tokens.tail.mkString(" ")
      println(s"query: $query")
      val tweets = pager.pagedRequest(pager.client.query(query))
      dumpTweets(new File(filename), tweets, false)
      println(s"${tweets.size} tweets saved to file $filename")
    }
  }
}

