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

  val divider = "-" * 75
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
  val allOWTweets = handleMapSubset.values.filter(_._2 == "Overweight").map(_._1.tweets.length).sum
  val allNOTweets = handleMapSubset.values.filter(_._2 == "Not overweight").map(_._1.tweets.length).sum

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
        val pso = resOWTweets.length.toDouble / numOW.toDouble
        val po = numOW.toDouble / numHandles.toDouble
        val ps = resTweets.length.toDouble / numHandles.toDouble
        val pos = pso * po / ps
        val psn = resNOTweets.length / numNO.toDouble
        val pn = numNO / numHandles.toDouble
        val pns = psn * pn / ps

        val to = numOWTweets.toDouble / allOWTweets
        val tn = numNOTweets.toDouble / allNOTweets

        println(divider)
        println(s"# accounts containing '$search': ${resTweets.size} / $numHandles")
        println(s"# ow accounts containing '$search': ${resOWTweets.length} / $numOW")
        println(s"# non-ow accounts containing '$search': ${resNOTweets.length} / $numNO")
        println(divider)
        // println(s"# tweets, all accounts containing '$search': ${numOWTweets+numNOTweets}")
        // println(s"# tweets, ow accounts that containing '$search': $numOWTweets")
        // println(s"# tweets, non-ow accounts containing '$search': $numNOTweets")
        // println(divider)
        println(f"Probability of ow given '$search': $pos%1.3f")
        println(f"Probability of non-ow given '$search': $pns%1.3f")
        println(f"Relative odds of ow given '$search': ${pos/po}%1.3f")
        println(f"Relative odds of non-ow given '$search': ${pns/pn}%1.3f")
        println(divider)
        println(f"Rel. freq. of '$search' in ow accts: ${to/tn}%1.3f")
        println(f"Rel. freq. of '$search' in non-ow accts: ${tn/to}%1.3f")
        println(f"Bits of info in '$search': ${log2(Seq(to/tn, tn/to).max)}%1.3f")
        println(divider)
        println()

        if (resTweets.nonEmpty) {
          val hWidth = resTweets.map(_._1.length).max
          val lWidth = "Not overweight".length + 1
          val tWidth = resTweets.map(_._3.length.toString.length).max
          val localDivider = "-" * (hWidth + lWidth + tWidth + 15)

          println(localDivider)
          println(("%-"+hWidth+"s").format("handle") + "\t" +
            ("%-"+lWidth+"s").format("label") + "\t" +
            "number tweets")
          println(localDivider)
          resTweets.sortBy(acct => (acct._3, - acct._2.length)).foreach { resIdx =>
            val (twAcHandle, tweets, lbl) = resIdx
            println(("%-"+hWidth+"s").format(twAcHandle) + "\t" +
              ("%-"+lWidth+"s").format(lbl) + "\t" +
              ("%-"+tWidth+"s").format(tweets.length)
            )
          }
          println(localDivider)
          println()
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
    val len = commandMap.keys.map(_.length).max
    val line = "-" * 60
    println(line)
    println("Enter regex search terms. Commands:")
    commandMap.foreach(cmd => println(("%-"+len+"s").format(cmd._1) + s"\t${cmd._2}"))
    println(line)
    println()
  }

  def log2(x: Double): Double = scala.math.log(x) / scala.math.log(2)
}