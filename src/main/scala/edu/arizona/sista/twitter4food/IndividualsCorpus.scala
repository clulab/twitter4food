package edu.arizona.sista.twitter4food

import java.io.File
import scala.collection.JavaConversions._

/**
 * Created by dfried on 5/6/15.
 */
class IndividualsCorpus(val baseDirectory: String, val trainingFraction: Double = 0.75, val randomSeed: Int = 1234, val numToTake: Option[Int] = Some(500)) {
  // baseDirectory should have one folder for each state
  // each state folder contains a single file per user, containing tweets from that user

  val stateDirs: Array[File] = { new File(baseDirectory).listFiles }

  val dirsByState = (for {
    stateDir <- stateDirs
  } yield (stateDir.getName -> stateDir)).toMap

  val tweetFilesByState: Map[String, Array[File]] = dirsByState.mapValues {
    stateDir => numToTake match {
        case None => stateDir.listFiles
        case Some(k) => stateDir.listFiles.sortBy(file => - file.length()).take(k)
    }
  }

  lazy val tweetsByUserByState: Map[String, Map[String, Seq[Tweet]]] = (for {
    (state, tweetFiles) <- tweetFilesByState
    _ = { println(state) }
    parsedTweetFiles = tweetFiles.map({ tweetFile => (tweetFile.getName, TweetParser.parseTweetFile(tweetFile.getAbsolutePath())) }).toMap
  } yield (state -> parsedTweetFiles)).toMap

  def splitTweetsTrainingAndTesting(tweetsByUser: Map[String, Seq[Tweet]]): (Map[String, Seq[Tweet]], Map[String, Seq[Tweet]]) = {
    val N = tweetsByUser.size
    val numTraining = (N * trainingFraction).toInt
    val numTesting = N - numTraining
    require(numTesting > 0, "numTesting is 0")
    require(numTraining > 0, "numTraining is 0")

    val (training, testing) = this.synchronized {
      util.Random.setSeed(randomSeed)
      // first sort by username so it's deterministic, then shuffle by seed
      val shuffled = util.Random.shuffle(tweetsByUser.toSeq.sortBy(_._1))
      (shuffled.take(numTraining), shuffled.drop(numTraining))
    }
    (training.toMap, testing.toMap)
  }

  // split each state into training and testing sets of tweets (grouped by user) by number of users
  private lazy val splitTweets = tweetsByUserByState.mapValues(splitTweetsTrainingAndTesting).toMap // convert to map because of lazy eval

  lazy val allTweets = tweetsByUserByState
  lazy val trainingTweets = splitTweets.mapValues(_._1)
  lazy val testingTweets = splitTweets.mapValues(_._2)
}

