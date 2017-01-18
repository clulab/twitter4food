package org.clulab.twitter4food.util

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.FileSystems

import com.typesafe.config.ConfigFactory
import org.slf4j.LoggerFactory

object PrintTokens {

  val logger = LoggerFactory.getLogger(this.getClass)
  val config = ConfigFactory.load
  val base = config.getString("classifiers.overweight.rawTokens")
  val sep = FileSystems.getDefault.getSeparator

  /**
    * Prints each account's tweets (one per line) to its own file.
    * Tweets are pre-tokenized (so no newlines w/i text).
    */
  def writeTokens(accounts: Seq[Seq[String]], loc: String): Unit = {
    val locFile = new File(loc)
    if (!locFile.exists) locFile.mkdir()
    accounts.zipWithIndex.foreach{ case (tweets, i) =>
      val fileName = s"$loc$i.txt"
      val writer = new BufferedWriter(new FileWriter(fileName))
      writer.write(tweets.mkString("\n"))
      writer.close()
    }
  }

  /**
    * Load all the tweets pertaining to a given variable ("overweight" by default), and print them to
    * train/test folders for each variable value. Database choices are "overweight", "human", "gender".
    */
  def main(args: Array[String]): Unit = {
    val dataset = if(args.isEmpty) "overweight" else args.head

    logger.info("Loading Twitter accounts")
    val labeledAccts = FileUtils.load(config.getString(s"classifiers.$dataset.data"))
      .toSeq
      .filter(_._1.tweets.nonEmpty)

    // Scale number of accounts equally so that weights aren't too biased against any one variable value
    val allLabels = labeledAccts.unzip._2.toSet
    val desiredProps = for (lbl <- allLabels) yield lbl -> 1.0 / allLabels.size
    val subsampled = Utils.subsample(labeledAccts, desiredProps.toMap)

    // Make sure equal number of each label in both train and test
    val lblToAccts = subsampled.groupBy(_._2)
    val texts = lblToAccts.map{ case (lbl, accountsWithLabels) =>
      lbl -> accountsWithLabels.map{ case (account, acctLabel) => account.tweets.map(_.text) }
    }
    // All variable values should have equal length
    val numInTest = (texts.head._2.length * 0.8).toInt + 1

    logger.info("Writing tokens in LSTM-readable format")

    val trainFile = new File(s"$base${sep}train")
    if (! trainFile.exists) trainFile.mkdir()
    val testFile = new File(s"$base${sep}test")
    if (! testFile.exists) testFile.mkdir()

    texts.foreach{ case (lbl, text) =>
      // folderNames should not contain whitespace
      val folderName = lbl.replaceAll("[^a-zA-Z0-9]+", "")
      writeTokens(text.slice(0, numInTest), s"$base${sep}train$sep$folderName")
      writeTokens(text.slice(numInTest, text.length), s"$base${sep}test$sep$folderName")
    }
  }
}