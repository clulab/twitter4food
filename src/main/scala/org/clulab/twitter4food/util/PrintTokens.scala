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
  def writeTokens(accounts: Map[Long, Seq[String]], loc: String): Unit = {
    val pb = new me.tongfei.progressbar.ProgressBar("printing", 100)
    pb.start()
    pb.maxHint(accounts.size)

    val locFile = new File(loc)
    if (!locFile.exists) locFile.mkdir()
    accounts.foreach{ case (id, tweets) =>
      val fileName = s"$loc$sep$id.txt"
      val writer = new BufferedWriter(new FileWriter(fileName))
      writer.write(tweets.mkString("\n"))
      writer.close()
      pb.step()
    }

    pb.stop()
  }

  /**
    * Load all the tweets pertaining to a given variable ("overweight" by default), and print them to
    * train/test folders for each variable value. Database choices are "overweight", "human", "gender".
    */
  def main(args: Array[String]): Unit = {
    val dataset = if(args.isEmpty) "overweight" else args.head

    logger.info("Loading Twitter accounts")
    val labeledAccts = FileUtils.loadTwitterAccounts(config.getString(s"classifiers.$dataset.data")).toSeq

    logger.info("Writing tokens in LSTM-readable format")

    labeledAccts.groupBy(_._2).foreach{ case (lbl, acctsWithLabels) =>
      // folderNames should not contain whitespace
      val folderName = lbl.replaceAll("[^a-zA-Z0-9]+", "")
      val texts = acctsWithLabels.map(al => al._1.id -> al._1.tweets.map(_.text)).toMap
      writeTokens(texts, s"$base$sep$folderName")
    }
  }
}