package org.clulab.twitter4food.data

import java.io.{BufferedWriter, File, FileWriter}
import java.net.URL

import com.typesafe.config.ConfigFactory
import org.apache.commons.io.FilenameUtils
import org.slf4j.LoggerFactory
import org.clulab.twitter4food.twitter4j.TwitterAPI
import org.clulab.twitter4food.util.FileUtils

import scala.sys.process._
import scala.util.Try

object ProfileImagesFromHandles {

  val logger = LoggerFactory.getLogger(this.getClass)

  case class PIConfig(variable: String)

  def parseArgs(args: Array[String]): PIConfig = {
    val parser = new scopt.OptionParser[PIConfig]("profilePics") {
      opt[String]('v', "variable") action { (x, c) =>
        c.copy(variable = x)} text "Variable to use"
    }
    val opts = parser.parse(args, PIConfig(variable = "overweight"))

    opts.get
  }

  def retrieveProfilePics(names: Seq[Long]): Map[Long, String] = {
    val numProcesses = 16
    val chunkSize = names.length / numProcesses

    val pics = for {
      thread <- (0 until numProcesses).par
      api = new TwitterAPI(thread)
      startIdx = thread * chunkSize
      lastIdx = if (thread + 1 == numProcesses) names.length else (thread + 1) * chunkSize
      i <- (startIdx until lastIdx).seq
      _ = println(s"$thread:${i-startIdx}/${lastIdx-startIdx} ${names(i)}")
      fetched = api.fetchProfilePic(names(i))
      if fetched.nonEmpty && fetched != Option("default")
    } yield names(i) -> fetched.get

    pics.seq.toMap
  }

  /**
    * Get the profile picture URLs and pictures for handles; user must choose which variable to collect
    *
    * @param args
    */
  def main (args: Array[String]): Unit = {
    val piConfig = parseArgs(args)
    val config = ConfigFactory.load

    val outf = new File(config.getString(s"classifiers.${piConfig.variable}.profile_pic_urls"))
    val outDir = config.getString(s"classifiers.${piConfig.variable}.twitterImages")
    val od = new File(outDir)
    if (!od.exists()) od.mkdir

    val handles = FileUtils.loadTwitterAccounts(config.getString(s"classifiers.${piConfig.variable}.data"))
      .toSeq
      .map{ case (acct, _) => acct.handle -> acct.id }
      .toMap

    val found = retrieveProfilePics(handles.values.toSeq)

    val writer = new BufferedWriter(new FileWriter(outf, true))

    found.foreach{case (id, url) => writer.write(s"$id\t$url\n")}

    writer.close()

    // Twitter's robots.txt asks for 1 s between requests
    val twitterDelay = 1010

    val pb = new me.tongfei.progressbar.ProgressBar("downloading", 100)
    pb.start()
    pb.maxHint(found.size)

    // Go through each user's files and try to download numToTake
    // This is intentionally not parallel to avoid spamming the server and getting blacklisted
    found.foreach { case (id, u) =>
      val userDirName = s"$outDir/$id"
      val userDir = new File(userDirName)
      if (!userDir.exists()) userDir.mkdir

      val url = new URL(u)
      val ext = FilenameUtils.getExtension(url.getPath)
      val photoLoc = s"$userDir/profile.$ext"

      val didItWork = Try(url.#>(new File(photoLoc)).!!) // the system command to download
      Thread.sleep(twitterDelay)

      // if at first we don't succeed, try try again just once in case the server was just overloaded
      if (didItWork.isFailure) {
        Try(url.#>(new File(photoLoc)).!!)
        Thread.sleep(twitterDelay)
      }
      pb.step()
    }
    pb.stop()

  }
}