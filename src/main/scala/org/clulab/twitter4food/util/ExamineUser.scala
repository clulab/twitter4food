package org.clulab.twitter4food.util

import java.io.File

import scala.util.Random
import com.typesafe.config.ConfigFactory
import jline.console.ConsoleReader
import jline.console.history.FileHistory
import org.clulab.struct.Lexicon
import org.clulab.twitter4food.featureclassifier.ClassifierImpl
import org.clulab.twitter4food.struct.TwitterAccount
import org.slf4j.LoggerFactory

/**
  * Gives a summary of a user's tweet relevance.
  */
object ExamineUser extends App {

  val logger = LoggerFactory.getLogger(this.getClass)
  val config = ConfigFactory.load()

  val history = new FileHistory(new File(System.getProperty("user.home"), ".examineuser"))
  sys addShutdownHook {
    history.flush() // we must flush the file before exiting
  }

  val r = new Random()

  val reader = new ConsoleReader
  reader.setPrompt(">>> ")

  var users = Map[TwitterAccount,String]()
  var lexicon = Set[String]()

  printCorpora

  while (users.isEmpty) {
    reader.readLine match {
      case "1" => // overweight
        logger.info("Loading users")
        users = FileUtils.load(config.getString("classifiers.overweight.data"))
        lexicon = loadLexicons(Set("Overweight", "Not overweight"), "overweight")
      case "2" => // human
        logger.info("Loading users")
        users = FileUtils.load(config.getString("classifiers.human.data"))
        lexicon = loadLexicons(Set("human", "org"), "human")
      case "3" => // gender
        logger.info("Loading users")
        users = FileUtils.load(config.getString("classifiers.gender.data"))
        lexicon = loadLexicons(Set("F", "M"), "gender")
      case "" => printCorpora
    }
  }

  println("Loading dictionaries")

  val handleMap = users.map{ case (acct, lbl) => stripAt(acct.handle) -> (acct, lbl) }
  assert(handleMap.nonEmpty, "Failed to load users!")

  val numHandles = handleMap.size

  reader.setHistory(history)

  var running = true

  printCommands

  while(running) {
    reader.readLine match {
      case "" => ()
      case ":random" =>
        val rando = handleMap.toSeq(r.nextInt(numHandles))
        val (ta, lbl) = rando._2
        println(summarize(ta, lbl))
      case ":exit" =>
        running = false
      case q =>
        val handle = stripAt(q)
        if(handleMap contains handle) {
          val (ta, lbl) = handleMap(handle)
          println(summarize(ta, lbl))
        } else {
          println(s"$q not found!")
        }
    }
  }

  // manual terminal cleanup
  reader.getTerminal().restore()
  reader.shutdown()



  def printCorpora: Unit = {
    println("[1] overweight classifier corpus")
    println("[2] human classifier corpus")
    println("[3] gender classifier corpus")
  }

  def printCommands: Unit = {
    println(":random => randomly select user")
    println(":exit => exit system")
  }

  /**
    * Returns a user handle minus the initial '@', if one exists
    */
  def stripAt(s: String): String = s.replaceFirst("@","")

  /**
    * Returns a set of relevant terms, given a set of labels and classifier type.
    */
  def loadLexicons(labelSet: Set[String], ctype: String): Set[String] = {
    logger.info(s"Loading $ctype lexicons")
    val lexiconMap = ClassifierImpl.populateLexiconList(labelSet, ctype)
    val l = lexiconMap map {
      case (k, v) => (k, v.map(fileName => {
        val lexName = fileName.substring(fileName.lastIndexOf("/") + 1,
          fileName.indexOf("."))
        (lexName, Lexicon.loadFrom[String](fileName))
      }).toMap)
    }
    val merged = l.values.flatMap(_.values).flatMap(_.keySet).toSet
    merged
  }

  /**
    * Returns a summary of a Twitter Account based on a dictionary of relevant terms
    */
  def summarize(ta: TwitterAccount, lbl: String, tweetsToDisplay: Int = 25): String = {
    val sb = new StringBuilder
    sb.append(s"Handle: ${ta.handle}\n")
    sb.append(s"Label: $lbl\n")
    sb.append(s"Name: ${ta.name}\n")
    sb.append(s"Description: ${ta.description}\n")
    // normal tweets (not replies or addressed tweets) w/o repeats
    val normals = ta.normalTweets.groupBy(_.text.take(40)).map(_._2.head).toSeq
    sb.append(s"# tweets: ${normals.length}\n")

    if (ta.tweets.nonEmpty) {
      val relevance = normals.map(t => t -> t.text.split("\\s+").count(lexicon.contains)).toMap
      val relevantTerms = relevance.values.toSeq
      val relevantPerTweet = relevantTerms.sum.toFloat / relevantTerms.length
      val percentRelevant = relevantTerms.count(_ > 0).toFloat / relevantTerms.length * 100.0
      sb.append(f"Relevant terms per tweet: $relevantPerTweet%1.3f\n")
      sb.append(f"tweets with > 0 relevant terms: $percentRelevant%1.1f\n")
      sb.append(f"tweets with > 0 relevant terms: $percentRelevant%1.1f%%\n")

      val mostRelevant = relevance.toSeq.sortBy(_._2).reverse.take(tweetsToDisplay).sortBy(_._1.createdAt)
      sb.append("Most relevant tweets:\n")
      mostRelevant.foreach(t => sb.append(s"\t[${t._1.createdAt}] ${t._1.text}\n"))
    }
    sb.toString
  }
}