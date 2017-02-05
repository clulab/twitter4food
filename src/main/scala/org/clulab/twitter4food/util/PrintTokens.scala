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
      val fileName = s"$loc$sep$i.txt"
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
    val train = FileUtils.load(config.getString(s"classifiers.$dataset.trainingData")).toSeq
    val dev = FileUtils.load(config.getString(s"classifiers.$dataset.trainingData")).toSeq
    val test = FileUtils.load(config.getString(s"classifiers.$dataset.trainingData")).toSeq

    logger.info("Writing tokens in LSTM-readable format")

    val trainFile = new File(s"$base${sep}train")
    if (! trainFile.exists) trainFile.mkdir()
    val devFile = new File(s"$base${sep}dev")
    if (! devFile.exists) devFile.mkdir()
    val testFile = new File(s"$base${sep}test")
    if (! testFile.exists) testFile.mkdir()

    train.groupBy(_._2).foreach{ case (lbl, acctsWithLabels) =>
      // folderNames should not contain whitespace
      val folderName = lbl.replaceAll("[^a-zA-Z0-9]+", "")
      val texts = acctsWithLabels.map(_._1.tweets.map(_.text))
      writeTokens(texts, s"$base${sep}train$sep$folderName")
    }
    dev.groupBy(_._2).foreach{ case (lbl, acctsWithLabels) =>
      // folderNames should not contain whitespace
      val folderName = lbl.replaceAll("[^a-zA-Z0-9]+", "")
      val texts = acctsWithLabels.map(_._1.tweets.map(_.text))
      writeTokens(texts, s"$base${sep}dev$sep$folderName")
    }
    test.groupBy(_._2).foreach{ case (lbl, acctsWithLabels) =>
      // folderNames should not contain whitespace
      val folderName = lbl.replaceAll("[^a-zA-Z0-9]+", "")
      val texts = acctsWithLabels.map(_._1.tweets.map(_.text))
      writeTokens(texts, s"$base${sep}test$sep$folderName")
    }
  }
}