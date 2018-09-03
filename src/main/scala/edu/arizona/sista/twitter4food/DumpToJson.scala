package edu.arizona.sista.twitter4food

import java.io._
import java.net._

object DumpToJson {
  def postToSolr(connUrl: String)(data: String) = {
    val conn = new URL(connUrl).openConnection
    conn.setDoOutput(true)
    conn.setDoInput(true)
    conn.setRequestProperty("Accept-Charset", "UTF-8")
    conn.setRequestProperty("Content-Type", "application/json")

    val output = new OutputStreamWriter(conn.getOutputStream)

    try {
      // output.write("commit=true")
      output.write(data)
      output.flush()
      val reader = new BufferedReader(new InputStreamReader(conn.getInputStream))
      reader.close
    } catch {
      case e: Throwable => println(e)
    } finally {
      output.close
    }
  }

  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
      try { op(p) } finally { p.close() }
  }

  def postFileToSolr(tweetFileName: String) = {

    val ldaModels: Map[TokenType, LDA] = Map(
      AllTokens -> LDA.load("/TokenType.scala"),
      FoodTokens -> LDA.load("/TokenType.scala"),
      HashtagTokens -> LDA.load("/TokenType.scala"),
      FoodHashtagTokens -> LDA.load("/TokenType.scala")
    )

    val source = Utils.loadFile(tweetFileName)

    val sentimentClassifier: Some[SentimentClassifier] = Some(SentimentClassifier.train(
      Some("C:/Users/Laptop/Documents/GitHub/twitter4food/src/main/resources/edu/arizona/sista/twitter4food/sentiment/happy.tweets"),
      Some("C:/Users/Laptop/Documents/GitHub/twitter4food/src/main/resources/edu/arizona/sista/twitter4food/sentiment/random.tweets"),
      Some("C:/Users/Laptop/Documents/GitHub/twitter4food/src/main/resources/edu/arizona/sista/twitter4food/sentiment/sad.tweets")))

    val tweetparser = new TweetParser(sentimentClassifier, ldaModels)
    val groupedLines = source.getLines.grouped(3)

    for ((block, blockIndex) <- tweetparser.parseBlockOfTweets(groupedLines).grouped(1000).zipWithIndex) {
      val jsonRep = JacksonWrapper.serialize(block.toList)
      println(s"posting block $blockIndex")
      postToSolr("http://localhost:8983/solr/twitter4food/update/json?commit=true")(jsonRep)
      println("complete")
    }
  }

  def main(args: Array[String]) {
    if (args.isEmpty) {
      println("pass the name of the tweet file as the first argument")
    } else {
      val sourceFile = args(0)
      postFileToSolr(sourceFile)
    }
  }
}
