package edu.arizona.sista.twitter4food

import java.io.File
import org.apache.commons.io.FilenameUtils

/**
 * Created by dfried on 5/12/15.
 */
class LabelledIndividualsCorpus(val csvFileName: String, val tweetDirectory: String) {
  val labelsByUsername: Map[String, Int] = LabelledIndividualsCorpus.getOverweightLabelsFromFile(csvFileName)

  val tweetParser = new MinimalTweetParser

  val tweets = for {
    userFile <- new File(tweetDirectory).listFiles.toSeq
    username = FilenameUtils.removeExtension(userFile.getName)
    if (labelsByUsername.contains(username))
    label = labelsByUsername(username)
    tweets = tweetParser.parseTweetFile(userFile.getAbsolutePath)
  } yield IndividualsTweets(tweets, username, Some(label), None)

}

object LabelledIndividualsCorpus {
  // mapping from
  def getOverweightLabelsFromFile(csvFileName: String): Map[String, Int] = {
    val source = io.Source.fromFile(csvFileName).getLines.toSeq
    (for {
      line <- source.drop(1)
      splits = line.split(",")
      username = splits(1).trim.drop(1) // drop @ sign
      overweightLabel = splits(9).trim match {
        case "Fit" => 0
        case "Overweight" => 1
      }
    } yield (username -> overweightLabel)).toMap
  }
}
