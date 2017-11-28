package org.clulab.twitter4food.util

import java.io.{BufferedWriter, File, FileWriter}

import org.apache.commons.io.FilenameUtils
import org.clulab.twitter4food.struct.FeatureExtractor.filterTags
import org.clulab.twitter4food.struct.TwitterAccount
import org.slf4j.LoggerFactory

import scala.collection.mutable.ArrayBuffer

object Tokenize {

  case class TokenizeConfig(inFile: String = "", outFile: Option[String] = None, isThreeLine: Boolean = false)

  def tokenizeTwoLines(filename: String, outFile: Option[String]): Unit = {
    val logger = LoggerFactory.getLogger(this.getClass)

    val tokenizedFN = if (outFile.isEmpty) {
      val tokenizedPath = FilenameUtils.getPrefix(filename) +
        FilenameUtils.getPathNoEndSeparator(filename) +
        "_tokenized/"
      tokenizedPath + FilenameUtils.getName(filename)
    } else outFile.get

    // check if untokenized file is older for appropriate warning
    val untokFile = new File(filename)
    val tokFile = new File(tokenizedFN)

    if (untokFile.exists & tokFile.exists & untokFile.lastModified() < tokFile.lastModified()) {
      logger.warn(s"$tokenizedFN is newer than $filename!")
    }

    val accounts = FileUtils.loadTwitterAccounts(filename).par

    val pb = new me.tongfei.progressbar.ProgressBar("Tokenize", 100)
    pb.start()
    pb.maxHint(accounts.size)
    pb.setExtraMessage("Tokenizing...")

    val letters = "[a-zA-Z]".r

    val tokenizedTweetsWithLabels: Seq[(TwitterAccount, String)] = (for {
      (account, lbl) <- accounts.toSeq
    } yield {

      // Only English tweets with words
      val englishTweets = account.tweets.filter(t =>
        t.lang != null & t.lang == "en" &
          t.text != null & t.text != "" &
          letters.findFirstMatchIn(t.text).nonEmpty
      )

      // Filter out stopwords
      val filteredTweets = for {
        t <- englishTweets
      } yield {
        val tt = Tokenizer.annotate(t.text)
        val ft = filterTags(tt).mkString(" ")
        t.copy(text = ft)
      }

      val tokenizedDescription = {
        val tt = Tokenizer.annotate(account.description)
        filterTags(tt).mkString(" ")
      }

      pb.step

      // Same account but with tokenized tweets
      account.copy(description = tokenizedDescription, tweets = filteredTweets) -> lbl
    }).seq

    pb.stop

    val (tokenizedTweets, labels) = tokenizedTweetsWithLabels.unzip

    FileUtils.saveToFile(tokenizedTweets, labels, tokenizedFN, append = false)
  }

  def tokenizeThreeLines(filename: String, outFile: Option[String]): Unit = {
    val logger = LoggerFactory.getLogger(this.getClass)

    val tokenizedFN = if (outFile.isEmpty) {
      val tokenizedPath = FilenameUtils.getPrefix(filename) +
        FilenameUtils.getPathNoEndSeparator(filename) +
        "_tokenized/"
      tokenizedPath + FilenameUtils.getName(filename)
    } else outFile.get

    // check if untokenized file is older for appropriate warning
    val untokFile = new File(filename)
    val tokFile = new File(tokenizedFN)

    if (untokFile.exists & tokFile.exists & untokFile.lastModified() < tokFile.lastModified()) {
      logger.warn(s"$tokenizedFN is newer than $filename!")
    }

    val file = scala.io.Source.fromFile(filename)
    val lines = file.getLines.toList
    file.close

    val userInfo = new ArrayBuffer[String]()
    val tweetInfo = new ArrayBuffer[String]()
    val texts = new ArrayBuffer[String]()

    var count = 0
    var lang = ""
    val pb = new me.tongfei.progressbar.ProgressBar("Tokenize", 100)
    pb.start
    pb.maxHint(lines.length)
    pb.setExtraMessage("Loading...")

    lines.foreach { line =>
      count match {
        case 0 =>
          lang = line.stripLineEnd.split("\t").last
          if (lang == "en") userInfo.append(line)
        case 1 => if (lang == "en") tweetInfo.append(line)
        case 2 => if (lang == "en") texts.append(line)
      }
      count += 1
      count %= 3

      pb.step
    }
    pb.stop

    val pb2 = new me.tongfei.progressbar.ProgressBar("Tokenize", 100)
    pb2.start
    pb2.maxHint(texts.length)
    pb2.setExtraMessage("Tokenizing...")

    val tokenizedTweets: Seq[String] = (for {
      text <- texts.par
    } yield {
      val tt = Tokenizer.annotate(text)
      val ft = filterTags(tt).mkString(" ")
      pb2.step
      ft
    }).seq

    pb2.stop

    val writer = new BufferedWriter(new FileWriter(tokFile, false))
    assert(tokenizedTweets.length == userInfo.length)
    assert(tokenizedTweets.length == tweetInfo.length)

    (userInfo, tweetInfo, tokenizedTweets).zipped.foreach{ case (u, tw, tt) => writer.write(s"$u\n$tw\n$tt\n") }

    writer.close
  }

  def main(args: Array[String]): Unit = {
    def parseArgs(args: Array[String]): TokenizeConfig = {
      val parser = new scopt.OptionParser[TokenizeConfig]("lda") {
        opt[String]('f', "inFile") action { (x, c) =>
          c.copy(inFile = x)
        } text "which file to tokenize"
        opt[String]('o', "outFile") action { (x, c) =>
          c.copy(outFile = Option(x))
        } text "location for result"
        opt[Unit]('t', "isThreeLine") action { (x, c) =>
          c.copy(isThreeLine = true)
        } text ""
        help("help") text "prints this usage text"
      }
      parser.parse(args, TokenizeConfig()).get
    }

    val params = parseArgs(args)
    if (params.inFile == "") {
      throw new RuntimeException("File to be tokenized must be specified using -f")
    }

    if (params.isThreeLine)
      tokenizeThreeLines(params.inFile, params.outFile)
    else
      tokenizeTwoLines(params.inFile, params.outFile)
  }
}