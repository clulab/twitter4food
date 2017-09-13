package org.clulab.twitter4food.util

import java.io.{BufferedWriter, File, FileWriter}

import com.typesafe.config.ConfigFactory
import org.slf4j.LoggerFactory
import scala.collection.JavaConverters._

object DumpToFlat extends App {
  case class Opts(
    corpus: String = "overweight",
    minTweets: Int = 0,
    maxTweets: Option[Int] = None,
    minWds: Option[Int] = None)

  def parseArgs(args: Array[String]): Opts = {
    val parser = new scopt.OptionParser[Opts]("dumpToJson") {
      head("dumpToJson", "0.x")
      opt[String]('c', "corpus") action { (x, c) =>
        c.copy(corpus = x)
      } text "which corpus {overweight, human, gender}"
      opt[Int]('n', "min") action { (x, c) =>
        c.copy(minTweets = x)
      } text "minimum number of tweets to print"
      opt[Int]('x', "max") action { (x, c) =>
        c.copy(maxTweets = Option(x))
      } text "maximum number of tweets to print"
      opt[Int]('s', "minWds") action { (x, c) =>
        c.copy(minWds = Option(x))
      } text "minimum message size"
    }

    val opts = parser.parse(args, Opts())

    if(opts.isEmpty) throw new IllegalArgumentException(s"args '${args.mkString(" ")}' are not supported!")

    opts.get
  }

  val logger = LoggerFactory.getLogger(this.getClass)
  val params = parseArgs(args)
  val config = ConfigFactory.load
  val sep = java.io.File.separator

  val labelMap = config
    .getStringList(s"classifiers.${params.corpus}.possibleLabels")
    .asScala
    .toList
    .reverse
    .zipWithIndex
    .toMap
  def mapLabel(textLabel: String): Int = labelMap(textLabel)

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
    .filter(_._1.tweets.length >= params.minTweets)

  val flattened = labeledAccts.map(_._1.toFlat(params.maxTweets, params.minWds)).mkString("\n")

  val flatOut = new BufferedWriter(new FileWriter(config.getString(s"classifiers.${params.corpus}.data_flat"), false))
  flatOut.write(flattened)
  flatOut.close()

  val labelOut = new BufferedWriter(new FileWriter(config.getString(s"classifiers.${params.corpus}.data_labels"), false))
  val labels = labeledAccts.map{ case (acct, label) => s"${acct.handle}\t${mapLabel(label)}\n"}.mkString("\n")
  labelOut.write(labels)
  labelOut.close()
}