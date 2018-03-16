package org.clulab.twitter4food.data

import java.io.{BufferedWriter, File, FileWriter}

import com.typesafe.config.ConfigFactory
import org.clulab.twitter4food.twitter4j.TwitterAPI
import org.clulab.twitter4food.util.FileUtils
import org.slf4j.LoggerFactory

case class TIUConfig(variable: String = "diabetes")

/**
  * Retrieve both Twitter-internal media URLs and Instagram-domain external URLs for each account in existing corpus
  */
object TwitterImageURLs {
  val logger = LoggerFactory.getLogger(this.getClass)

  def parseArgs(args: Array[String]): TIUConfig = {
    val parser = new scopt.OptionParser[TIUConfig]("TIUExtraction") {
      head("Choose a variable from the set {overweight, ow2, diabetes, human, gender}")
      arg[String]("variable") action { (x, c) => c.copy(variable = x) } text "variable to use"
    }

    val opts = parser.parse(args, TIUConfig())

    if(opts.isEmpty) throw new IllegalArgumentException(s"args ${args.mkString(" ")} are not supported!")

    opts.get
  }

  def retrieveURLs(ids: Seq[Long]): Map[Long, (Seq[String], Seq[String])] = {
    val numProcesses = 18
    val chunkSize = ids.length / numProcesses

    val pics = for {
      thread <- (0 until numProcesses).par
      api = new TwitterAPI(thread)
      i <- thread * chunkSize until (thread + 1) * chunkSize
    } yield {
      logger.debug(s"fetching ${ids(i)}")
      ids(i) -> api.fetchImages(ids(i))
    }

    pics.seq.toMap
  }

  def main(args:Array[String]): Unit = {
    val config = ConfigFactory.load
    val opts = parseArgs(args)

    // make directory for twitter picture URLs to go into, if necessary
    val twid = config.getString(s"classifiers.${opts.variable}.twitterImageURLs")
    val twf = new File(twid)
    if(! twf.exists) twf.mkdir()

    // make directory for external picture URLs to go into, if necessary
    val exid = config.getString(s"classifiers.${opts.variable}.externalImageURLs")
    val exf = new File(exid)
    if(! exf.exists) exf.mkdir()

    // read account IDs in (all else is thrown away, not very efficient)
    val accts = FileUtils.loadTwitterAccounts(config.getString("classifiers.overweight.data"))
      .keys
      .filter(_.tweets.nonEmpty)
      .map(_.id)
      .toSeq

    // actually get the URLs using the Twitter API (takes a long time b/c API limits)
    val images = retrieveURLs(accts)

    // write URLs to file to download later
    images.foreach { case (id, (pics, urls)) =>
      val iFile = new File(twid + File.separator + id.toString + ".txt")
      val iWriter = new BufferedWriter(new FileWriter(iFile, true))
      pics.foreach(pic => iWriter.write(pic + "\n"))
      iWriter.close()

      val eFile = new File(exid + File.separator + id.toString + ".txt")
      val eWriter = new BufferedWriter(new FileWriter(eFile, true))
      urls.foreach(url => eWriter.write(url + "\n"))
      eWriter.close()
    }
  }
}