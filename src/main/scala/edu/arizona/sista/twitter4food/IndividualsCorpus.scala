package edu.arizona.sista.twitter4food

import java.io.{File, PrintWriter}
import org.apache.commons.io.FilenameUtils
import com.github.tototoshi.csv.CSVReader

/**
 * Created by dfried on 5/6/15.
 */
// tweets for an individual
case class IndividualsTweets(val tweets: List[Tweet], val username: String, val label: Option[Int], val state: Option[String])

class IndividualsCorpus(val baseDirectory: String, val annotationFile: String, val annotatedTestingFraction: Double = 0.8, val randomSeed: Int = 1234, val numToTake: Option[Int] = Some(500), val excludeUsersWithMoreThan: Option[Int] = None, val organizationsFile: Option[String] = None) extends Serializable {
  // baseDirectory should have one folder for each state
  // each state folder contains a single file per user, containing tweets from that user

  val stateDirs: Array[File] = { new File(baseDirectory).listFiles }

  val dirsByState = (for {
    stateDir <- stateDirs
    if (stateDir.isDirectory)
  } yield (stateDir.getName -> stateDir)).toMap

  val organizationUsernames: Option[Set[String]] = organizationsFile.map(filename =>
    (for {
      line <- Utils.loadFile(filename).getLines
      tokens = line.split("\t")
      username = tokens(0)
      strippedUsername = if (username.charAt(0) == '@') username.substring(1) else username
      label = tokens(1).toInt
      if (label == 1)
    } yield strippedUsername).toSet
  )

  // map(identity) because http://stackoverflow.com/questions/17709995/notserializableexception-for-mapstring-string-alias
  val tweetFilesByState: Map[String, Array[File]] = dirsByState.mapValues(_.listFiles.filter(file =>
    organizationUsernames match {
      case Some(set) => ! set.contains(usernameForFile(file))
      case None => true
    }
  )).map(identity)

  val userAnnotations: Map[String, Int] = IndividualsCorpus.labelsFromAnnotationFile(annotationFile, header = true)


  val (testingUsers, devUsers) = {
    val random = new util.Random(randomSeed)
    val groupedUsers: Seq[Seq[(String, Int)]] = userAnnotations.toSeq.groupBy(_._2).toSeq.sortBy(_._1).map(_._2)
    val shuffledGroups = groupedUsers.map(random.shuffle(_))
    val numTestingPerClass: Seq[Int] = shuffledGroups.map(list => (list.size * annotatedTestingFraction).toInt)
    val testing = (shuffledGroups zip numTestingPerClass).flatMap {
      case (group, n) => group.take(n).map(_._1)
    }
    val dev = (shuffledGroups zip numTestingPerClass).flatMap {
      case (group, n) => group.drop(n).map(_._1)
    }
    (testing.toSet, dev.toSet)
  }

  private def usernameForFile(file: File) = {
    FilenameUtils.removeExtension(file.getName)
  }

  // map usernames to filtered tweets (annotated users excluded, and up to K users by number of tweets descending)
  private def getTrainingTweets(files: Array[File]): Map[String, Seq[Tweet]] = {
    // sort the tweet files by number of tweets, descending
    val filesAndNumTweets: Seq[(File, Int)] = files.toSeq.map(file => {
      val source = io.Source.fromFile(file)
      val numLines = source.getLines.size
      source.close
      (file, numLines / 3)
    })

    // sort the files by number of tweets, descending, and only take those which we don't have annotations for and (if excludeUsersWithMoreThan is set) have fewer than that number of tweets (since users with many tweets are probably advertisers
    var trainingFiles = (excludeUsersWithMoreThan match {
      case None => filesAndNumTweets
      case Some(k) => filesAndNumTweets.filter(_._2 <= k)
    }).sortBy(- _._2).map(_._1).filter(file => ! testingUsers.contains(usernameForFile(file)) && !devUsers.contains(usernameForFile(file)))

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
    tweets = tweetParser.parseTweetFile(io.Source.fromFile(file)).toList
    label = userAnnotations(username)
  } yield IndividualsTweets(tweets, username, Some(label), state=None)).toSeq

  val (testingTweets: List[IndividualsTweets], devTweets: List[IndividualsTweets]) = {
    val tweetParser = new MinimalTweetParser
    (getAnnotatedTweetsMatchingUsernames(tweetParser, testingUsers).toList, getAnnotatedTweetsMatchingUsernames(tweetParser, devUsers).toList)
  }

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

    reader.close()

    labelMap
  }
}

