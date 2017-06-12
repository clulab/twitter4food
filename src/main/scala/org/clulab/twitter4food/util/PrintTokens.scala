package org.clulab.twitter4food.util

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.FileSystems

import com.typesafe.config.ConfigFactory
import org.clulab.twitter4food.struct.{FeatureExtractor, TwitterAccount}
import org.slf4j.LoggerFactory

object PrintTokens {

  val logger = LoggerFactory.getLogger(this.getClass)
  val config = ConfigFactory.load
  val base = config.getString("classifiers.overweight.rawTokens")
  val sep = FileSystems.getDefault.getSeparator

  case class PrintTokensConfig(variable: String = "overweight", domainAdaptation: Boolean = false)

  def parseArgs(args: Array[String]): PrintTokensConfig = {
    val parser = new scopt.OptionParser[PrintTokensConfig]("tokenPrinter") {
      opt[String]('v', "variable") action { (x, c) =>
        c.copy(variable = x)} text "Variable to use"
      opt[Unit]('d', "domainAdaptation") action { (x, c) =>
        c.copy(domainAdaptation = true)} text "Use domain adaptation"
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

  /**
    * Prints each account's tweets (one per line) to its own file.
    * Tweets are pre-tokenized (so no newlines w/i text).
    */
  def writeTokens(accounts: Seq[TwitterAccount], loc: String, fe: Option[FeatureExtractor]): Unit = {
    val pb = new me.tongfei.progressbar.ProgressBar("printing", 100)
    pb.start()
    pb.maxHint(accounts.size)

    val locFile = new File(loc)
    if (!locFile.exists) locFile.mkdir()

    accounts.foreach{ account =>
      val fileName = s"$loc$sep${account.id}.txt"
      val writer = new BufferedWriter(new FileWriter(fileName))
      if (fe.nonEmpty) {
        val gender = getGender(account, fe.get)
        val age = getAge(account, fe.get)
        val lines = account.tweets.map{ tweet =>
          val rt = if (tweet.isRetweet) "rt" else "nrt"
          s"($gender, $age, $rt)\t${tweet.text}"
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
    * train/test folders for each variable value. Database choices are "overweight", "human", "gender".
    */
  def main(args: Array[String]): Unit = {
    val printConfig = parseArgs(args)

    logger.info("Loading Twitter accounts")
    val labeledAccts = FileUtils.loadTwitterAccounts(config.getString(s"classifiers.${printConfig.variable}.data")).toSeq

    logger.info("Writing tokens in LSTM-readable format")

    labeledAccts.groupBy(_._2).foreach{ case (lbl, acctsWithLabels) =>
      // folderNames should not contain whitespace
      val folderName = lbl.replaceAll("[^a-zA-Z0-9]+", "")
      val texts = acctsWithLabels.map(_._1)
      val fe = if (printConfig.domainAdaptation)
        Option(new FeatureExtractor(useRT = true, useGender = true, useAge = true, variable = printConfig.variable))
      else
        None

      writeTokens(texts, s"$base$sep$folderName", fe)
    }
  }
}