package org.clulab.twitter4food.util

import java.io.File

import com.typesafe.config.ConfigFactory
import jline.console.ConsoleReader
import jline.console.history.FileHistory
import org.clulab.twitter4food.struct.TwitterAccount
import org.slf4j.LoggerFactory
import org.clulab.struct.Counter
import java.io.PrintWriter

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
  var full = false

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

  var numOfTweets = 0
  var numOfRetweets = 0
  var numOfNotRetweets = 0
  val resTweets = handleMap.keys.flatMap{ twAcHandle =>
    val data = handleMap(twAcHandle)
    val ta = data._1
    val lbl = data._2
    val retweets = for(t <- ta.tweets) yield {
      numOfTweets += 1
      if(t.isRetweet) {
        numOfRetweets += 1
        Some(t)
      }
      else {
        numOfNotRetweets += 1
        None
      }
    }
    
    retweets
   }
  
  val retweetsFile = new PrintWriter(new File("retweets.txt"))
  for (t <- resTweets) {
    if(t.nonEmpty) { retweetsFile.write(s"${t.get.text}\n")  }
  }
  retweetsFile.close()
  logger.info(s"Total no of Tweets : ${numOfTweets}\nNo. of Retweets : ${numOfRetweets}\nNo. of Regular Tweets : ${numOfNotRetweets}")
  logger.info("Retweets file written")
  
//  val highInfoWords = getAllHighInfoTweetWords(handleMap) // getAllHighInfoTweetWords(handleMapSubset)
//  val highInfoWordsFile = new PrintWriter(new File("HighInfoWords.txt"))
//  for((w,info,freq) <- highInfoWords){
//    highInfoWordsFile.write(s"$w\t$info\t$freq\n")
//  }
//  highInfoWordsFile.write(divider)
//  highInfoWordsFile.close()
//  logger.info("HighFreq words written to HighInfoWords.txt")
  
  /*var running = true
  val reader = new ConsoleReader
  printHelp()
  reader.setPrompt(">>> ")

  while(running) {
    reader.readLine match {
      case "" => ()
      case ":help" => printHelp()
      case ":verbose" => verbose = ! verbose
      case ":full" => full = ! full
      case ":exit" =>
        running = false
      case query =>
        if (full) search(handleMapSubset, query) else search(handleMap, query)
    }
  }*/

  def getAllHighInfoTweetWords(handleMap: Map[String, (TwitterAccount, String)]) : Seq[(String, Double, Int)] = {
    
    logger.info("Vocab construction begin")
    
//    var vocab = Set[String]()
    val vocabCounter = new Counter[String]
    for(i <- handleMap.toSeq){
      val (handle, ta_lbl) = (i._1, i._2)
      val ta = ta_lbl._1
      for (t <- ta.tweets){
        val words = t.text.trim.split(" +")
        words.foreach(word => vocabCounter.incrementCount(word, 1))
      }
      
    }
    logger.info("Vocab construction end")
    logger.info(s"Size of vocab ${vocabCounter.size}")
    
    logger.info("Downsizing the vocab")
    val totalCount = vocabCounter.toSeq.unzip._2.sum
    var cumulativeCount = 0.0
    var vocabSubset = Set[String]()
    for((word,count) <- vocabCounter.toSeq.sortBy(- _._2)){
      if(cumulativeCount / totalCount <= 0.9){
        cumulativeCount += count
        vocabSubset += word
      }
    }
    logger.info(s"Size of the subset of the vocab based on frequency : ${vocabSubset.size}")
        
    logger.info("Begun tweet tokenization")
    val tokenizedTweets = (for(twAcHandle <- handleMap.keys) yield { 
      val data = handleMap(twAcHandle)
      val ta = data._1.tweets
      val tweetsTokenized = for (t <- data._1.tweets) yield {
        t.text.trim.split(" +")
      }
      (twAcHandle, tweetsTokenized)
    }).toMap
    logger.info("End tweet tokenization")
    
    val pb = new me.tongfei.progressbar.ProgressBar("wordsProcessed", vocabSubset.size)
    pb.setExtraMessage("Processing...")
    pb.start()  
    
    val wordsAndBitsofInfo = for ((w,i) <- vocabSubset.toSeq.zipWithIndex) yield {
      pb.step()  
      val resTweets = handleMap.keys.par.flatMap{ twAcHandle =>
            val data = handleMap(twAcHandle)
            val ta = data._1
            val lbl = data._2
//          val regex = w.r
            val tweets = tokenizedTweets(twAcHandle).filter ( x =>  x.contains(w) ).map { x => x.mkString(" ") }

            if (tweets.isEmpty) None else Some(twAcHandle, tweets, lbl)
        }.toSeq.seq
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

        val bitsOfInfo = log2(Seq(to/tn, tn/to).max)
        val scaledBitsOfInfo = bitsOfInfo *  (resOWTweets.size + resNOTweets.size)
        (w,scaledBitsOfInfo, (resOWTweets.size + resNOTweets.size))
    }
   
    pb.stop()
    
    val wordsAndBitsFinite = wordsAndBitsofInfo.filterNot( _._2.isInfinity).sortBy(-_._2)
    val sz = wordsAndBitsofInfo.size
    wordsAndBitsFinite.filter(_._3 > 25).take((sz * 0.2).toInt)
  }
  /**
    * Returns a user handle minus the initial '@', if one exists
    */
  def stripAt(s: String): String = s.replaceFirst("@","")

  def printHelp(): Unit = {
    val commandMap = Map(
      ":help" -> "Print this message",
      ":verbose" -> "Toggle printing all relevant tweets (default false)",
      ":full" -> "Toggle searching full dataset (default false)",
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

  def search(accts: Map[String, (TwitterAccount, String)], query: String): Unit = {
    val resTweets = accts.keys.par.flatMap{ twAcHandle =>
      val data = accts(twAcHandle)
      val ta = data._1
      val lbl = data._2
      val regex = query.r

      val tweets = ta.tweets.filter( x => regex.findAllIn(x.text).nonEmpty)
      if (tweets.isEmpty) None else Some(twAcHandle, tweets, lbl)
    }.toSeq.seq

    val numHandles = accts.size
    val numOW = accts.values.filter(_._2 == "Overweight").toSeq.length
    val numNO = accts.values.filter(_._2 == "Not overweight").toSeq.length
    val allOWTweets = accts.values.filter(_._2 == "Overweight").map(_._1.tweets.length).sum
    val allNOTweets = accts.values.filter(_._2 == "Not overweight").map(_._1.tweets.length).sum

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
    println(s"# accounts containing '$query': ${resTweets.size} / $numHandles")
    println(s"# ow accounts containing '$query': ${resOWTweets.length} / $numOW")
    println(s"# non-ow accounts containing '$query': ${resNOTweets.length} / $numNO")
    println(divider)
    // println(s"# tweets, all accounts containing '$query': ${numOWTweets+numNOTweets}")
    // println(s"# tweets, ow accounts that containing '$query': $numOWTweets")
    // println(s"# tweets, non-ow accounts containing '$query': $numNOTweets")
    // println(divider)
    println(f"Probability of ow given '$query': $pos%1.3f")
    println(f"Probability of non-ow given '$query': $pns%1.3f")
    println(f"Relative odds of ow given '$query': ${pos/po}%1.3f")
    println(f"Relative odds of non-ow given '$query': ${pns/pn}%1.3f")
    println(divider)
    println(f"Rel. freq. of '$query' in ow accts: ${to/tn}%1.3f")
    println(f"Rel. freq. of '$query' in non-ow accts: ${tn/to}%1.3f")
    println(f"Bits of info in '$query': ${log2(Seq(to/tn, tn/to).max)}%1.3f")
    println(divider)
    println()

    if (resTweets.nonEmpty && verbose) {
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