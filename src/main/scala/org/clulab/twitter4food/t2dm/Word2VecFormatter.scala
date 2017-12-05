package org.clulab.twitter4food.t2dm

import java.io.{BufferedReader, FileReader, PrintWriter}

import cmu.arktweetnlp.Tagger.TaggedToken
import org.clulab.twitter4food.struct.FeatureExtractor._
import org.clulab.twitter4food.util.{FileUtils, Utils, Tokenizer}

case class W2VConfig(inFile: String = "", outFile: String = "", isThreeLine: Boolean = true, tokenize: Boolean = true)

/**
  * Load a 2- or 3-line formatted Twitter file and write it in a word2vec-readable format.
  */
object Word2VecFormatter extends App {
  def parseArgs(args: Array[String]): W2VConfig = {
    val parser = new scopt.OptionParser[W2VConfig]("w2vFormatter") {
      arg[String]("inFile") action { (x, c) =>
        c.copy(inFile = x)
      } text "which file to convert"
      arg[String]("outFile") action { (x, c) =>
        c.copy(outFile = x)
      } text "where to put the converted file"
      opt[Unit]('2', "isTwoLine") action { (x, c) =>
        c.copy(isThreeLine = false)
      } text "for two-line input format"
      opt[Unit]('t', "tokenized") action { (x, c) =>
        c.copy(tokenize = false)
      } text "use if already tokenized"
    }

    parser.parse(args, W2VConfig()).get
  }

  val config = parseArgs(args)

  val writer = new PrintWriter(config.outFile)

  val tweets = if (config.isThreeLine)
    FileUtils.loadThreeLineTexts(config.inFile)
  else
    FileUtils.loadTwoLineTexts(config.inFile)

  val tokenizedTweets = if (config.tokenize) {
    val pb = new me.tongfei.progressbar.ProgressBar("Tokenize", 100)
    pb.start()
    pb.maxHint(tweets.length)
    pb.setExtraMessage("Tokenizing...")

    val tokenized = tweets.par.map { tweet =>
      val tt = Tokenizer.annotate(tweet)
      val ft = filterTags(tt, lowerCase = true).mkString(" ")
      pb.step()
      ft
    }.seq

    pb.stop()

    tokenized
  } else tweets

  tokenizedTweets.foreach(tweet => writer.write(s"$tweet\n"))

  writer.close()
}
