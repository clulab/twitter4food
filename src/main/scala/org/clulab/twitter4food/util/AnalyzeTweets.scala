package org.clulab.twitter4food.util

import java.io.File

import com.typesafe.config.ConfigFactory
import jline.console.ConsoleReader
import jline.console.history.FileHistory
import org.clulab.twitter4food.struct.TwitterAccount
import org.slf4j.LoggerFactory

/**
  * Gives a summary of a user's tweet relevance.
  */
object AnalyzeTweets extends App {

  val logger = LoggerFactory.getLogger(this.getClass)
  val config = ConfigFactory.load()

  val history = new FileHistory(new File(System.getProperty("user.home"), ".analyzetweets"))
  sys addShutdownHook {
    history.flush() // we must flush the file before exiting
  }

  val divider = "-" * 50

  var users = Map[TwitterAccount,String]()

  logger.info("Loading users")
  users = FileUtils.load(config.getString("classifiers.overweight.data"))

  println("Loading dictionaries")

  var handleMap = users.map{ case (acct, lbl) => stripAt(acct.handle) -> (acct, lbl) }
  assert(handleMap.nonEmpty, "Failed to load users!")

  val handlesToAnalyze = Array("angiewhoohoo", "MaeghanLucinda", "JulianaYaz", "Kathneko", "YOB_JayDaisy", "queenachan", "checkoutfashion", "Graziella_a", "Banditgangnate", "YunJae8686", "alfritz04", "YungPauline", "yakdon", "ceejaydeleon", "Hannah_Barnett0", "steveendranata", "DrDamodhar", "Emily11949309", "LeumasHon", "sarlynclements", "Jo_RDPT16", "JonathanOneLife", "kat0417", "JessCording", "Lottie_Lamour", "siShingalinG", "sachadetti", "ScouseMattSmith", "jonathanlegate", "kanahina", "ellwoodz", "bl3berry", "jackiehagland", "oh_mandy93", "nohyunji", "Myatzee", "GarnerStyle", "mchefajaychopra", "MissMelanieD", "Beksville", "edibleASH", "Parsnip_Pete", "Gloriahillching", "JenniferBible1", "Spezzie", "GluttonEire")  
  val handleMapSubset = handleMap.filterKeys (handlesToAnalyze.toSet)

  val numHandles = handleMapSubset.size

  var running = true
  val reader = new ConsoleReader
  reader.setPrompt("Enter the search term >>> ")

  while(running) {
    reader.readLine match {
      case "" => ()
      case ":exit" => 
        running = false
      case search => 
        val resTweets = handleMapSubset.keys.flatMap{ twAcHandle =>
            val data = handleMapSubset(twAcHandle)
            val ta = data._1
            val lbl = data._2
            val regex = search.r
            
            val tweets = ta.tweets.filter( x => regex.findAllIn(x.text).nonEmpty)
            if (tweets.isEmpty) None else Some(twAcHandle, tweets, lbl)
        }.toSeq
        val resOWTweets = resTweets.filter(_._3 == "Overweight")
        val resNOTweets = resTweets.filter(_._3 == "Not overweight")
        val numOWTweets = (for(x <- resOWTweets) yield x._2.length).sum
        val numNOTweets = (for(x <- resNOTweets) yield x._2.length).sum

        println(s"Search results for $search")
        println(divider)
        println(s"Number of accounts that contain mention $search : ${resTweets.size}")
        println(s"Number of tweets in all accounts that mention  $search : ${numOWTweets+numNOTweets}")
        println(s"Number of Overweight accounts that mention $search : ${resOWTweets.size}")
        println(s"Number of Tweets in Overweight accounts that mention $search : $numOWTweets")
        println(s"Number of Not Overweight accounts that mention $search : ${resNOTweets.size}")
        println(s"Number of Tweets in Overweight accounts that mention $search : $numNOTweets")
        println(divider)
        println()

        if (resTweets.nonEmpty) {
          val hWidth = resTweets.map(_._1.length).max
          val lWidth = "Not overweight".length
          val tWidth = resTweets.map(_._3.length.toString.length).max
          println("%"+hWidth+"s".format("handle") + "\t" +
            "%"+hWidth+"s".format("label") + "\t" +
            "%"+hWidth+"s".format("number tweets"))
          println(divider)
          resTweets.foreach { resIdx =>
            val (twAcHandle, tweets, lbl) = resIdx
            println(("%"+hWidth+"s").format(stripAt(twAcHandle)) + "\t" +
              ("%"+lWidth+"s").format(lbl) + "\t" +
              ("%"+tWidth+"s").format(tweets.length)
            )
          }
          println(divider)
          println()
          resTweets.map(_._2).foreach(ts => ts.foreach(println(_)))
        }
    }
  }

  /**
    * Returns a user handle minus the initial '@', if one exists
    */
  def stripAt(s: String): String = s.replaceFirst("@","")
}