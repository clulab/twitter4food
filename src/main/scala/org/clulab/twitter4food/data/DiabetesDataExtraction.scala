package org.clulab.twitter4food.data

import com.typesafe.config.ConfigFactory
import org.slf4j.LoggerFactory

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

    var steps = 0

    val accts = for {
      thread <- (0 until numProcesses).par
      api = new TwitterAPI(thread)
      startIdx = thread * chunkSize
      lastIdx = if (thread + 1 == numProcesses) names.length else (thread + 1) * chunkSize
      i <- (startIdx until lastIdx).seq
    } yield {
      println(s"$steps/${names.length} ${names(i)}")
      val fetched = api.fetchAccount(names(i), fetchTweets = true)
      steps += 1
      fetched
    }

    accts.seq
  }

  def main(args: Array[String]) {

    val config = ConfigFactory.load
    val outputFile = config.getString("classifiers.diabetes.data")
    val inputFileStr = config.getString("classifiers.diabetes.handles")

    val inputFile = scala.io.Source.fromFile(inputFileStr)
    val lines = inputFile.getLines

    val pairs = for (line <- lines) yield {
      // Parse line
      val tuple = line.stripLineEnd.split("\t")
      val handle = sanitizeHandle(tuple.head)
      val label = tuple.last
      (handle, label)
    }

    val labeledAccounts = pairs.toMap
    val handles = labeledAccounts.keys.toSeq

    inputFile.close()

    val accounts = retrieveAccts(handles)
    val nonNull = accounts.filterNot(_ == null)
    val labels = nonNull.map(acct => labeledAccounts.getOrElse(sanitizeHandle(acct.handle), "NULL"))

    logger.info("DiabetesDataExtraction: Saving to file...")
    FileUtils.saveToFile(nonNull, labels, outputFile)
  }
}
