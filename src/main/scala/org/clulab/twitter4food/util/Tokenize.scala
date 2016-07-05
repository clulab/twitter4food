package org.clulab.twitter4food.util

import java.io.File

import org.apache.commons.io.FilenameUtils
import org.clulab.twitter4food.struct.FeatureExtractor.filterTags
import org.clulab.twitter4food.struct.TwitterAccount
import org.slf4j.LoggerFactory

object Tokenize {
  def main(args: Array[String]): Unit = {
    val logger = LoggerFactory.getLogger(this.getClass)

    if (args.length < 1) println("Please specify path and file to tokenize")

    val tokenizedPath = FilenameUtils.getPrefix(args.head) +
      FilenameUtils.getPathNoEndSeparator(args.head) +
      "_tokenized/"
    val tokenizedFN = tokenizedPath + FilenameUtils.getName(args.head)

    // check if untokenized file is older for appropriate warning
    val untokFile = new File(args.head)
    val tokFile = new File(tokenizedFN)

    if (untokFile.exists & tokFile.exists & untokFile.lastModified() < tokFile.lastModified()) {
      logger.warn(s"$tokenizedFN is newer than ${args.head}!")
    }

    val accounts = FileUtils.load(args.head)

    val pb = new me.tongfei.progressbar.ProgressBar("Tokenize", 100)
    pb.start()
    pb.maxHint(accounts.size)
    pb.setExtraMessage("Tokenizing...")

    val tokenizedTweetsWithLabels: Seq[(TwitterAccount, String)] = for {
      (account, lbl) <- accounts.toSeq
    } yield {

      // Only English tweets with words
      val englishTweets = account.tweets.filter( t =>
        t.lang != null & t.lang == "en" &
          t.text != null & t.text != ""
      )

      // Filter out stopwords
      val filteredTweets = for {
        t <- englishTweets
      } yield {
        val tt = Tokenizer.annotate(t.text.toLowerCase)
        val ft = filterTags(tt).map(_.token).mkString(" ")
        t.copy(text = ft)
      }

      val tokenizedDescription = {
        val tt = Tokenizer.annotate(account.description.toLowerCase)
        filterTags(tt).map(_.token).mkString(" ")
      }

      pb.step

      // Same account but with tokenized tweets
      account.copy(description = tokenizedDescription, tweets = filteredTweets) -> lbl
    }

    pb.stop

    val (tokenizedTweets, labels) = tokenizedTweetsWithLabels.unzip

    FileUtils.saveToFile(tokenizedTweets, labels, tokenizedFN, append = false)
  }
}