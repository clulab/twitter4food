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
  var verbose = false

  var users = Map[TwitterAccount,String]()

  logger.info("Loading users")
  users = FileUtils.load(config.getString("classifiers.overweight.data"))

  println("Loading dictionaries")

  var handleMap = users.map{ case (acct, lbl) => stripAt(acct.handle) -> (acct, lbl) }
  assert(handleMap.nonEmpty, "Failed to load users!")

  val handlesToAnalyze = Array("angiewhoohoo", "MaeghanLucinda", "JulianaYaz", "Kathneko", "YOB_JayDaisy", "queenachan", "checkoutfashion", "Graziella_a", "Banditgangnate", "YunJae8686", "alfritz04", "YungPauline", "yakdon", "ceejaydeleon", "Hannah_Barnett0", "steveendranata", "DrDamodhar", "Emily11949309", "LeumasHon", "sarlynclements", "Jo_RDPT16", "JonathanOneLife", "kat0417", "JessCording", "Lottie_Lamour", "siShingalinG", "sachadetti", "ScouseMattSmith", "jonathanlegate", "kanahina", "ellwoodz", "bl3berry", "jackiehagland", "oh_mandy93", "nohyunji", "Myatzee", "GarnerStyle", "mchefajaychopra", "MissMelanieD", "Beksville", "edibleASH", "Parsnip_Pete", "Gloriahillching", "JenniferBible1", "Spezzie", "GluttonEire")  
  val handleMapSubset = handleMap.filterKeys (handlesToAnalyze.toSet)

  val numHandles = handleMapSubset.size
  val numOW = handleMapSubset.values.filter(_._2 == "Overweight").toSeq.length
  val numNO = handleMapSubset.values.filter(_._2 == "Not overweight").toSeq.length

  var running = true
  val reader = new ConsoleReader
  printHelp()
  reader.setPrompt(">>> ")

  while(running) {
    reader.readLine match {
      case "" => ()
      case ":help" => printHelp()
      case ":verbose" => verbose = true
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

        // probabilities for Bayesian calculation. read as P(search term|overweight label), P(not overweight), etc.
        val pso = resOWTweets.length / numOW.toDouble
        val psn = resNOTweets.length / numNO.toDouble
        val po = numOW / numHandles.toDouble
        val pn = numNO / numHandles.toDouble
        val ps = resTweets.length / numHandles.toDouble
        val pos = pso * po / ps
        val pns = psn * pn / ps

        println(divider)
        println(s"# accounts that contain '$search': ${resTweets.size}")
        println(s"# overweight accounts that contain '$search': ${resOWTweets.length}")
        println(s"# non-overweight accounts that contain '$search': ${resNOTweets.length}")
        println(divider)
        println(s"# tweets in all accounts that contain '$search': ${numOWTweets+numNOTweets}")
        println(s"# tweets in overweight accounts that contain '$search': $numOWTweets")
        println(s"# tweets in non-overweight accounts that contain '$search': $numNOTweets")
        println(divider)
        println(s"Probability of overweight given '$search': $pos")
        println(s"Probability of non-overweight given '$search': $pso")
        println(s"Relative likelihood of overweight given '$search': ${pos/(pos+pso)}")
        println(divider)
        println()

        if (resTweets.nonEmpty) {
          val hWidth = resTweets.map(_._1.length).max
          val lWidth = "Not overweight".length + 1
          val tWidth = resTweets.map(_._3.length.toString.length).max
          println(s"${" " * (hWidth - "handle".length)}handle" + "\t" +
            s"${" " * (lWidth - "label".length)}label" + "\t" +
            "number tweets")
          println("-" * hWidth + lWidth + tWidth)
          resTweets.sortBy(_._3).foreach { resIdx =>
            val (twAcHandle, tweets, lbl) = resIdx
            println(("%"+hWidth+"s").format(twAcHandle) + "\t" +
              ("%"+lWidth+"s").format(lbl) + "\t" +
              ("%"+tWidth+"s").format(tweets.length)
            )
          }
          println(divider)
          if (verbose) {
            println(s"Matching tweets:\n${"-" * 16}\n")
            resTweets.map(_._2).foreach(ts => ts.foreach(println(_)))
            println(divider)
          }
          println()
        }
    }
  }

  /**
    * Returns a user handle minus the initial '@', if one exists
    */
  def stripAt(s: String): String = s.replaceFirst("@","")

  def printHelp(): Unit = {
    val commandMap = Map(
      ":help" -> "Print this message",
      ":verbose" -> "Toggle printing all relevant tweets (default false)",
      ":exit" -> "Exit"
    )
    val line = "-" * 60
    println(line)
    println("Enter regular expression search terms. Commands:")
    commandMap.foreach(cmd => println(s"${cmd._1}\t${cmd._2}"))
    println(line)
    println()
  }
}