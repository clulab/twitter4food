package org.clulab.twitter4food.data

import java.nio.file.{Paths, Files}

import sys.process._
import com.typesafe.config.ConfigFactory
import org.slf4j.LoggerFactory

import org.clulab.twitter4food.util.FileUtils
import org.clulab.twitter4food.util.Utils.sanitizeHandle
import org.clulab.twitter4food.struct.TwitterAccount
import org.clulab.twitter4food.twitter4j.TwitterAPI

case class DataExtractionConfig(variable: String = "diabetes")

/**
  * Author: Dane Bell
  *
  * A script for downloading the tweets of a file of twitter handles.
  * The file is assumed to have the format of
  * [handle]\t[classificationLabel]
  * on each line.
  */
object DataExtraction {
  val logger = LoggerFactory.getLogger(this.getClass)

  def parseArgs(args: Array[String]): DataExtractionConfig = {
    val parser = new scopt.OptionParser[DataExtractionConfig]("dataExtraction") {
      head("Choose a variable from the set {overweight, ow2, diabetes, human, gender}")
      arg[String]("variable") action { (x, c) =>
        c.copy(variable = x)} text "variable to use"
    }

    val opts = parser.parse(args, DataExtractionConfig())

    if(opts.isEmpty) throw new IllegalArgumentException(s"args ${args.mkString(" ")} are not supported!")

    opts.get
  }


  def retrieveAccounts[T](names: Seq[T], numProcesses: Int = 16): Seq[TwitterAccount] = {
    assert(names.isEmpty ||
      names.head.isInstanceOf[String] ||
      names.head.isInstanceOf[Long],
      "TwitterAccount identifiers must be Strings or Longs!")

    val chunkSize = names.length / numProcesses

    var steps = 0

    val accounts = for {
      thread <- (0 until numProcesses).par
      api = new TwitterAPI(thread)
      startIdx = thread * chunkSize
      lastIdx = if (thread + 1 == numProcesses) names.length else (thread + 1) * chunkSize
      i <- (startIdx until lastIdx).seq
    } yield {
      steps += 1
      println(s"$steps/${names.length} ${names(i)}")
      val fetched = names(i) match {
        case handle: String => api.fetchAccount(handle, fetchTweets = true)
        case id: Long => api.fetchAccount(id.toString, fetchTweets = true, isID = true)
      }
      fetched
    }

    accounts.seq
  }

  def main(args: Array[String]) {

    val deConfig = parseArgs(args)

    logger.info(s"Updating tweets for ${deConfig.variable} classification")

    val config = ConfigFactory.load
    val outputFile = config.getString(s"classifiers.${deConfig.variable}.data_raw")
    val inputFileStr = config.getString(s"classifiers.${deConfig.variable}.handles")

    // Save a copy of the existing accounts in case something goes wrong
    val corpusExists = Files.exists(Paths.get(outputFile))
    if (corpusExists) s"cp $outputFile ${outputFile + ".backup"}".!
    logger.info(s"Backup of existing file $outputFile saved to ${outputFile + ".backup"}")

    val previous: Seq[(TwitterAccount, String)] = if (corpusExists)
      FileUtils.loadTwitterAccounts(outputFile).toSeq
    else
      Nil
    val (prevAccounts, _) = previous.unzip

    val inputFile = scala.io.Source.fromFile(inputFileStr)
    val lines = inputFile.getLines

    val pairs = for (line <- lines) yield {
      // Parse line
      val tuple = line.stripLineEnd.split("\t")
      val handle = sanitizeHandle(tuple.head)
      val label = tuple.last
      (handle, label)
    }

    val labeledHandles = pairs.toMap
    val handles = labeledHandles.keys.toSeq

    inputFile.close()

    val accounts = retrieveAccounts(handles).filterNot(_ == null)
    val labels = accounts.map(acct => labeledHandles.getOrElse(sanitizeHandle(acct.handle), "NULL"))

    // Update existing accounts. Notice that we keep all the account's *new* information (e.g. description) which could
    // have changed. Likewise, we are keeping only the new labels.
    val updated = for (account <- accounts) yield {
      val query = prevAccounts.filter(_.id == account.id)
      if (query.nonEmpty)
        account.merge(query.head)
      else
        account
    }

    // Some old accounts may have
    //  1. changed their handles,
    //  2. been deactivated, or
    //  3. made private,
    // but we still want them. We will check IDs for updates and then just keep the rest as is.
    val updatedIds = updated.map(_.id)
    val lostIds = previous.map(_._1.id).filterNot(id => updatedIds.contains(id))

    val newLostAccounts = retrieveAccounts(lostIds).filterNot(_ == null)
    val (oldLostAccounts, lostLabels) = previous.filterNot{ case (acct, lbl) => updatedIds.contains(acct.id) }.unzip

    // Update existing accounts whose names have not been found. Notice that we prefer to keep all the account's *new*
    // information (e.g. description), which could have changed.
    val updatedLost = for (acct <- oldLostAccounts) yield {
      val query = newLostAccounts.filter(_.id == acct.id)
      if (query.nonEmpty)
        query.head.merge(acct)
      else
        acct
    }

    val allAccounts = updated ++ updatedLost
    val allLabels = labels ++ lostLabels
    val (nonEmptyAccounts, nonEmptyLabels) = allAccounts.zip(allLabels).filter(_._1.tweets.nonEmpty).unzip

    logger.info("DataExtraction: Saving to file...")
    FileUtils.saveToFile(nonEmptyAccounts, nonEmptyLabels, outputFile)
  }
}
