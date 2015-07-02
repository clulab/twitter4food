package edu.arizona.sista.twitter4food

import java.io.{File, PrintWriter}
import org.apache.commons.io.FilenameUtils
import com.github.tototoshi.csv.CSVReader

/**
 * Created by dfried on 5/6/15.
 */
// tweets for an individual
case class IndividualsTweets(val tweets: Seq[Tweet], val username: String, val label: Option[Int], val state: Option[String])

class IndividualsCorpus(val baseDirectory: String, val annotationFile: String, val annotatedTestingFraction: Double = 0.8, val randomSeed: Int = 1234, val numToTake: Option[Int] = Some(500)) {
  // baseDirectory should have one folder for each state
  // each state folder contains a single file per user, containing tweets from that user

  val stateDirs: Array[File] = { new File(baseDirectory).listFiles }

  val dirsByState = (for {
    stateDir <- stateDirs
  } yield (stateDir.getName -> stateDir)).toMap

  val tweetFilesByState: Map[String, Array[File]] = dirsByState.mapValues(_.listFiles)

  val userAnnotations: Map[String, Int] = IndividualsCorpus.labelsFromAnnotationFile(annotationFile, header = true)

  val random = new util.Random(randomSeed)


  val (testingUsers, devUsers) = {
    val shuffledUsers = random.shuffle(userAnnotations.keys.toSeq.sorted)

    val N = shuffledUsers.size
    val numTesting = (N * annotatedTestingFraction).toInt
    val numDev = N - numTesting

    require(numTesting > 0, "numTesting is 0")
    require(numDev > 0, "numDev is 0")

    (shuffledUsers.take(numTesting).toSet, shuffledUsers.drop(numTesting).toSet)
  }

  private def usernameForFile(file: File) = {
    FilenameUtils.removeExtension(file.getName)
  }

  // map usernames to filtered tweets (annotated users excluded, and up to K users by number of tweets descending)
  private def getTrainingTweets(files: Array[File]): Map[String, Seq[Tweet]] = {
    // sort the tweet files by number of tweets, descending
    val filesByNumTweets = files.sortBy(file => {
      val source = io.Source.fromFile(file)
      val numLines = source.getLines.size
      source.close
      - numLines
    })

    // get those which do not have annotations
    var trainingFiles = filesByNumTweets.filter(file => ! testingUsers.contains(usernameForFile(file)) && !devUsers.contains(usernameForFile(file)))

    // if we have a limit, take only up to that many
    numToTake.foreach(k => trainingFiles = trainingFiles.take(k))

    // parse them
    val parser = new MinimalTweetParser

    (for {
      file <- trainingFiles
      username = usernameForFile(file)
      tweets = parser.parseTweetFile(file.getAbsolutePath)
    } yield username -> tweets).toMap
  }

  private def getAnnotatedTweetsMatchingUsernames(tweetParser: MinimalTweetParser, usernames: Set[String]) = (for {
    file <- tweetFilesByState.values.flatten
    username = usernameForFile(file)
    if (usernames.contains(username))
    tweets = tweetParser.parseTweetFile(io.Source.fromFile(file))
    label = userAnnotations(username)
  } yield IndividualsTweets(tweets, username, Some(label), state=None)).toSeq

  private val tweetParser = new MinimalTweetParser
  lazy val testingTweets = getAnnotatedTweetsMatchingUsernames(tweetParser, testingUsers)
  lazy val devTweets = getAnnotatedTweetsMatchingUsernames(tweetParser, devUsers)

  lazy val trainingTweetsByState: Map[String, Map[String, Seq[Tweet]]] = (for {
    (state, tweetFiles) <- tweetFilesByState.par
  } yield (state -> getTrainingTweets(tweetFiles))).seq.toMap
}

object IndividualsCorpus {
  def labelsFromAnnotationFile(filename: String, header: Boolean = true): Map[String, Int] = {
    val reader = CSVReader.open(filename)

    val lines = reader.iterator.toList

    // map users to label (1 for overweight, 0 for not)
    val labelMap: Map[String, Int] = (for {
      fields <- if (header) lines.drop(1) else lines
      username = fields(0)
      label = fields(5) match {
        case "Overweight" => 1
        case "NotOverweight" => 0
        case label => throw new Exception(s"invalid individual label $label")
      }
    } yield username -> label).toMap

    labelMap
  }
}

