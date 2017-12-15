package org.clulab.twitter4food.util

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.FileSystems

import com.typesafe.config.ConfigFactory
import org.clulab.struct.Lexicon
import org.clulab.twitter4food.struct.{FeatureExtractor, Location, Tweet, TwitterAccount}
import org.slf4j.LoggerFactory

object PrintTokens {

  val logger = LoggerFactory.getLogger(this.getClass)
  val config = ConfigFactory.load
  val sep = FileSystems.getDefault.getSeparator

  case class PrintTokensConfig(variable: String = "overweight",
                               domainAdaptation: Boolean = false,
                               dictOnly: Boolean = false)

  def parseArgs(args: Array[String]): PrintTokensConfig = {
    val parser = new scopt.OptionParser[PrintTokensConfig]("tokenPrinter") {
      opt[String]('v', "variable") action { (x, c) =>
        c.copy(variable = x)} text "Variable to use"
      opt[Unit]('d', "domainAdaptation") action { (x, c) =>
        c.copy(domainAdaptation = true)} text "Use domain adaptation"
      opt[Unit]('l', "dictOnly") action { (x, c) =>
        c.copy(dictOnly = true)} text "Only print unigrams that appear in topic dictionaries"
    }
    val opts = parser.parse(args, PrintTokensConfig())

    opts.get
  }

  def getGender(account: TwitterAccount, fe: FeatureExtractor): String = {
    val acctGenderFirst = if (fe.genderAnnotation.nonEmpty)
      fe.genderAnnotation.get.get(account.id.toString)
    else None
    val acctGenderSecond = if (acctGenderFirst.isEmpty && fe.genderClassifier.nonEmpty)
      fe.genderClassifier.get.predict(account)
    else "UNK"
    acctGenderFirst.getOrElse(acctGenderSecond)
  }

  def getAge(account: TwitterAccount, fe: FeatureExtractor): String = {
    val ageExact = fe.ageAnnotation.get.get(account.id.toString)
    if (ageExact.nonEmpty) {
      val ae = ageExact.get.toDouble
      f"$ae%1.1f"
    } else "UNK"
  }

  def getLoc(tweet: Tweet, locs: Option[Seq[Location]]): (String, String, String, String) = {
    if (locs.isEmpty || locs.get.isEmpty) return ("", "", "", "")
    val loc = locs.get.find(l => l.id.toLong == tweet.id)
    if (loc.isEmpty || loc.get.venues.isEmpty) return ("", "", "", "")
    val placeType = loc.get.venues.head.types.head
    val placeName = loc.get.venues.head.name
    val lat = loc.get.llat
    val lng = loc.get.llng
    (placeType, placeName, lat.toString, lng.toString)
  }

  /**
    * Prints each account's tweets (one per line) to its own file.
    * Tweets are pre-tokenized (so no newlines w/i text).
    */
  def writeTokens(accounts: Seq[TwitterAccount],
                  loc: String,
                  fe: Option[FeatureExtractor],
                  dictOnly: Boolean = false): Unit = {

    val da = if (fe.nonEmpty) "_da" else ""
    val isDict = if (dictOnly) "_dictOnly" else ""
    val fullLoc = s"$loc$da$isDict"
    val locFile = new File(fullLoc)
    if (!locFile.exists) locFile.mkdir()

    val lexicons = if (dictOnly) {
      val lex = new Lexicon[String]()
      for {
        dict <- fe.get.allDicts.get
        word <- dict.keySet
      } {
        lex.add(word)
      }
      Option(lex)
    } else None

    val pb = new me.tongfei.progressbar.ProgressBar("printing", 100)
    pb.start()
    pb.maxHint(accounts.size)

    accounts.foreach{ account =>
      val fileName = s"$fullLoc$sep${account.id}.txt"
      val writer = new BufferedWriter(new FileWriter(fileName))
      if (fe.nonEmpty) {
        val gender = if (fe.get.useGender) getGender(account, fe.get) else "UNK"
        val age = if (fe.get.useAge) getAge(account, fe.get) else "UNK"
        val lines = account.tweets.flatMap{ tweet =>
          val rt = if (tweet.isRetweet) "rt" else "nrt"
          val noise = if (Utils.isNoise(tweet)) "spam" else "ham"
          val text = if (fe.get.dictOnly)
            tweet.text.split(" +").filter(lexicons.get.contains).mkString(" ")
          else
            tweet.text
          if (text == "")
            None
          else
            Option(s"($gender, $age, $rt, $noise)\t$text")
        }
        writer.write(lines.mkString("\n"))
      }
      else writer.write(account.tweets.map(_.text).mkString("\n"))
      writer.close()
      pb.step()
    }

    pb.stop()
  }

  /**
    * Load all the tweets pertaining to a given variable ("overweight" by default), and print them to
    * train/test folders for each variable value. Database choices are "overweight", "human", "gender", "diabetes".
    */
  def main(args: Array[String]): Unit = {
    val printConfig = parseArgs(args)
    val base = config.getString(s"classifiers.${printConfig.variable}.rawTokens")

    val baseDir = new File(base)
    if (!baseDir.exists) baseDir.mkdir()

    logger.info("Loading Twitter accounts")
    val labeledAccts = FileUtils.loadTwitterAccounts(config.getString(s"classifiers.${printConfig.variable}.data")).toSeq

    logger.info("Writing tokens in LSTM-readable format")

    val fe = if (printConfig.domainAdaptation && printConfig.variable == "diabetes")
      Option(new FeatureExtractor(useRT = true,
        variable = printConfig.variable,
        dictOnly = printConfig.dictOnly)
      )
    else if (printConfig.domainAdaptation)
      Option(new FeatureExtractor(useRT = true,
        useGender = true,
        useAge = true,
        variable = printConfig.variable,
        dictOnly = printConfig.dictOnly)
      )
    else
      None

    labeledAccts.groupBy(_._2).foreach{ case (lbl, acctsWithLabels) =>
      // folderNames should not contain whitespace
      val folderName = lbl.replaceAll("[^a-zA-Z0-9]+", "")
      val path = s"$base$sep$folderName"

      val dir = new File(path)
      if (!dir.exists) dir.mkdir()

      val texts = acctsWithLabels.map(_._1)

      writeTokens(texts, path, fe)
    }
  }
}