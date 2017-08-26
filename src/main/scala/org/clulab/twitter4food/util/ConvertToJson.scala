package org.clulab.twitter4food.util

import com.typesafe.config.ConfigFactory
import org.slf4j.LoggerFactory
import java.io.{BufferedWriter, File, FileWriter}

object ConvertToJson extends App {
  case class Opts(corpus: String = "overweight", toy: Boolean = false)

  def parseArgs(args: Array[String]): Opts = {
    val parser = new scopt.OptionParser[Opts]("convertToJson") {
      head("convertToJson", "0.x")
      opt[String]('c', "corpus") action { (x, c) =>
        c.copy(corpus = x)
      } text "which corpus {overweight, human, gender}"
      opt[Unit]('t', "toy") action { (x, c) =>
        c.copy(toy = true)
      } text "only include <= 10 tweets?"
    }

    val opts = parser.parse(args, Opts())

    if(opts.isEmpty) throw new IllegalArgumentException(s"args ${args.mkString(" ")} are not supported!")

    opts.get
  }

  val logger = LoggerFactory.getLogger(this.getClass)
  val params = parseArgs(args)
  val config = ConfigFactory.load
  val sep = java.io.File.separator

  val corpus = params.corpus.toLowerCase match {
    case "overweight" => "overweight"
    case "human" => "human"
    case "gender" => "gender"
    case other =>
      logger.warn(s"${params.corpus} not valid, defaulting to overweight")
      "overweight"
  }

  val labeledAccts = FileUtils.loadTwitterAccounts(config.getString(s"classifiers.${params.corpus}.data"))
    .toSeq
    .filter(_._1.tweets.nonEmpty)

  if (params.toy) {
    val imageLoc = config.getString(s"classifiers.${params.corpus}.twitterImages")
    val imageFolders = new File(imageLoc).listFiles
    val images = imageFolders.map { f =>
      val id = f.getName.toLong
      val acctImages: Seq[String] = if (! f.isDirectory) Nil else f.list
      id -> acctImages
    }.toMap

    val shortened = labeledAccts.map { case (acct, lbl) =>
      val acctImages = images.getOrElse(acct.id, Nil)
      val fewerTweets = acct.copy(
        tweets = acct.tweets.takeRight(10).zipWithIndex.map { case (t, i) =>
          val tweetImage = acctImages.lift(i).toList
          t.copy(images = tweetImage)
        }
      )
      (fewerTweets, lbl)
    }

    val out = new BufferedWriter(new FileWriter(config.getString(s"classifiers.${params.corpus}.data_json"), false))

    val allJson = shortened.map{ case (acct, lbl) => acct.toJson(label = Option(lbl))}.mkString("[\n", ",\n", "\n]")
    out.write(allJson)

    out.close()
  } else {
    // nothing for now
  }
}