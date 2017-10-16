package org.clulab.twitter4food.data

import com.typesafe.config.ConfigFactory
import org.slf4j.LoggerFactory
import me.tongfei.progressbar.ProgressBar

import org.clulab.twitter4food.util.FileUtils
import org.clulab.twitter4food.util.Utils.sanitizeHandle
import org.clulab.twitter4food.struct.TwitterAccount
import org.clulab.twitter4food.twitter4j.TwitterAPI

/**
  * Author: Dane Bell
  *
  * A script for downloading the tweets of a file of twitter handles.
  * The file is assumed to have the format of
  * [handle]\t[classificationLabel]
  * on each line.
  */
object DiabetesDataExtraction {
  val logger = LoggerFactory.getLogger(this.getClass)

  def retrieveAccts(names: Seq[String]): Seq[TwitterAccount] = {
    val numProcesses = 16
    val chunkSize = names.length / numProcesses

    val pb = new ProgressBar("Retrieving...", 100)
    pb.start()
    pb.maxHint(names.length)

    val accts = for {
      thread <- (0 until numProcesses).par
      api = new TwitterAPI(thread)
      startIdx = thread * chunkSize
      lastIdx = if (thread + 1 == numProcesses) names.length else (thread + 1) * chunkSize
      i <- (thread * chunkSize to lastIdx).seq
    } yield {
      pb.setExtraMessage(s"$thread:${i - startIdx}/${lastIdx - startIdx}\t${names(i)}")
      val fetched = api.fetchAccount(names(i), fetchTweets = true)
      pb.step()
      fetched
    }

    accts.seq
  }

  def main(args: Array[String]) {

    val config = ConfigFactory.load
    val outputFile = config.getString("diabetes.data")
    val inputFileStr = config.getString("diabetes.handles")

    val inputFile = scala.io.Source.fromFile(inputFileStr)
    val lines = inputFile.getLines

    val pairs = for (line <- lines) yield {
      // Parse line
      val tuple = line.stripLineEnd.split("\t")
      val handle = sanitizeHandle(tuple.head)
      val label = tuple.last
      (handle, label)
    }

    inputFile.close()

    val labeledAccounts = pairs.toMap
    val handles = labeledAccounts.keys.toSeq

    val accounts = retrieveAccts(handles)
    val labels = accounts.map(acct => labeledAccounts.getOrElse(acct.handle, "NULL"))

    logger.info("DiabetesDataExtraction: Saving to file...")
    FileUtils.saveToFile(accounts, labels, outputFile)

    println("\n\nDiabetesDataExtraction: Finished!")
  }
}
