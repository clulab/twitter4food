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
object AnalyzeTweets extends App {

  val logger = LoggerFactory.getLogger(this.getClass)
  val config = ConfigFactory.load()

  val history = new FileHistory(new File(System.getProperty("user.home"), ".examineuser"))
  sys addShutdownHook {
    history.flush() // we must flush the file before exiting
  }

  val r = new Random()

  var users = Map[TwitterAccount,String]()
  var lexicon = Set[String]()

  logger.info("Loading users")
  users = FileUtils.load(config.getString("classifiers.overweight.data"))
  lexicon = loadLexicons(Set("Overweight", "Not overweight"), "overweight")

  println("Loading dictionaries")

  var handleMap = users.map{ case (acct, lbl) => stripAt(acct.handle) -> (acct, lbl) }
  assert(handleMap.nonEmpty, "Failed to load users!")

  val handlesToAnalyze = Array("angiewhoohoo", "MaeghanLucinda", "JulianaYaz", "Kathneko", "YOB_JayDaisy", "queenachan", "checkoutfashion", "Graziella_a", "Banditgangnate", "YunJae8686", "alfritz04", "YungPauline", "yakdon", "ceejaydeleon", "Hannah_Barnett0", "steveendranata", "DrDamodhar", "Emily11949309", "LeumasHon", "sarlynclements", "Jo_RDPT16", "JonathanOneLife", "kat0417", "JessCording", "Lottie_Lamour", "siShingalinG", "sachadetti", "ScouseMattSmith", "jonathanlegate", "kanahina", "ellwoodz", "bl3berry", "jackiehagland", "oh_mandy93", "nohyunji", "Myatzee", "GarnerStyle", "mchefajaychopra", "MissMelanieD", "Beksville", "edibleASH", "Parsnip_Pete", "Gloriahillching", "JenniferBible1", "Spezzie", "GluttonEire")  
  val handleMapSubset = handleMap.filterKeys (handlesToAnalyze.toSet)

  val numHandles = handleMapSubset.size

  //  handleMap.retain(h => if(handlesToAnalyze.contains(h))
//  val handle = stripAt(q)
//  if(handleMap contains handle) {
//    val (ta, lbl) = handleMap(handle)
//    println(summarize(ta, lbl))
//  }
  var running = true
  val reader = new ConsoleReader
  reader.setPrompt("Enter the search term >>> ")

//  while(running) {
//    reader.readLine match {
//      case "" => ()
//      case ":random" =>
//        println(numHandles)
//        val rando = handleMapSubset.toSeq(r.nextInt(numHandles)) //r.nextInt(numHandles))
//        val (ta, lbl) = rando._2
//        println(summarize(ta, lbl))
//      case ":exit" =>
//        running = false
//      case q => 
//        val handle = stripAt(q)
//        if(handleMap contains handle) {
//          val (ta, lbl) = handleMapSubset(handle)
//          println(summarize(ta, lbl))
//        } else {
//          println(s"$q not found!")
//        }
//    }
//  }
//  running = true
  
  while(running) {
    reader.readLine match {
      case "" => ()
      case ":exit" => 
        running = false
      case search => 
        val resTweets = for(twAcHandle <- handleMapSubset.keys) yield {
            val data = handleMapSubset(twAcHandle)
            val ta = data._1
            val lbl = data._2
            
            val z = ta.tweets.head
            val tweets = ta.tweets.filter ( x => x.text.contains(search)  ) 
            (twAcHandle, tweets, lbl)
        }
        val resOWTweets = resTweets.filter(_._3.equals("Overweight"))
        val resNOTweets = resTweets.filter(_._3.equals("Not overweight"))
        val numOWTweets = (for(x <- resOWTweets) yield x._2.size).sum
        val numNOTweets = (for(x <- resNOTweets) yield x._2.size).sum

        println(s"Search results for $search")
        println(s"------------------------------------")
        println(s"Number of accounts that contain mention $search : ${resTweets.size}")
        println(s"Number of tweets in all accounts that mention  $search : ${numOWTweets+numNOTweets}")
        println(s"Number of Overweight accounts that mention $search : ${resOWTweets.size}")
        println(s"Number of Tweets in Overweight accounts that mention $search : ${numOWTweets}")
        println(s"Number of Not Overweight accounts that mention $search : ${resNOTweets.size}")
        println(s"Number of Tweets in Overweight accounts that mention $search : ${numNOTweets}")
        println(s"------------------------------------")
        
        resTweets.zipWithIndex.foreach {resIdx =>
          val idx = resIdx._2
          val res = resIdx._1
          val (twAcHandle, tweets, lbl) = (res._1, res._2, res._3)
          println(s"$idx\n--")
          println(s"Handle :  $twAcHandle")
          println(s"Label : $lbl")
          val twText = for (t <- tweets) yield {
            t.text
          }
//          println(s"Tweets : ${twText.mkString("\n")}")
          println(s"Number of tweets :  ${twText.size}")
          println("---")
        }
    }
  }
  
//  while(running) {
//    reader.readLine match {
//      case "" => ()
//      case ":random" =>
//        println(numHandles)
//        val rando = handleMap.toSeq(1) //r.nextInt(numHandles))
//        val (ta, lbl) = rando._2
//        println(summarize(ta, lbl))
//      case ":exit" =>
//        running = false
//      case q =>
//        val handle = stripAt(q)
//        if(handleMap contains handle) {
//          val (ta, lbl) = handleMap(handle)
//          println(summarize(ta, lbl))
//        } else {
//          println(s"$q not found!")
//        }
//    }
//  }

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
    sb.append(s"# tweets: ${normals.length}")

    if (ta.tweets.nonEmpty) {
      val relevance = normals.map(t => t -> t.text.split(" +").count(lexicon.contains)).toMap
      val relevantTerms = relevance.values.toSeq
      val relevantPerTweet = relevantTerms.sum.toFloat / relevantTerms.length
      val percentRelevant = relevantTerms.count(_ > 0).toFloat / relevantTerms.length * 100.0
      sb.append(f"Relevant terms per tweet: $relevantPerTweet%1.3f\n")
      sb.append(f"tweets with > 0 relevant terms: $percentRelevant%1.1f\n")

      val mostRelevant = relevance.toSeq.sortBy(_._2).reverse.take(tweetsToDisplay).sortBy(_._1.createdAt)
      sb.append("Most relevant tweets:\n")
      mostRelevant.foreach(t => sb.append(s"\t[${t._1.createdAt}] ${t._1.text}\n"))
    }
    sb.toString
  }
}