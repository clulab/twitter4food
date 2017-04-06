package org.clulab.twitter4food.data

import java.io.File
import java.net.URL

import com.typesafe.config.{Config, ConfigFactory}
import jline.console.ConsoleReader
import org.apache.commons.io.FilenameUtils
import org.slf4j.{Logger, LoggerFactory}

import scala.sys.process._
import scala.util.Try

/**
  * Download images from BigOven corresponding to recipes already downloaded using BigOvenRecipes
  */
object BigOvenImages {

  val config: Config = ConfigFactory.load
  val logger: Logger = LoggerFactory.getLogger(this.getClass)
  // Normal API rate is 500 requests / hour
  val delay = 7200
  // perturb delay by some amount
  val perturbAmount = 100
  val r = scala.util.Random
  // highest recipe ID (including private)
  val maxIdx: Int = config.getInt("bigOven.maxIdx")

  def main(args: Array[String]): Unit = {
    val reader = new ConsoleReader
    reader.setPrompt(">>> ")

    // get first index, so this job can be split among multiple servers
    var startIdx: Option[Int] = None
    while (startIdx.isEmpty) {
      println("Skip to index")
      reader.readLine match {
        case num if Try(num.toInt).isSuccess =>
          val candidate = num.toInt
          if (candidate < 1) startIdx = Option(1)
          else if (candidate > maxIdx) startIdx = Option(1)
          else startIdx = Option(candidate)
        case other => startIdx = Option(1)
      }
    }

    // get last index, so this job can be split among multiple servers
    var endIdx: Option[Int] = None
    while (endIdx.isEmpty) {
      println("End at index (inclusive)")
      reader.readLine match {
        case num if Try(num.toInt).isSuccess =>
          val candidate = num.toInt
          if (candidate < startIdx.get) endIdx = Option(maxIdx)
          else if (candidate > maxIdx) endIdx = Option(maxIdx)
          else endIdx = Option(candidate)
        case other => endIdx = Option(maxIdx)
      }
    }

    logger.info(s"Fetching ${startIdx.get} to ${endIdx.get}")

    // where to put pictures (and what pictures we already have)
    val outDir = config.getString("bigOven.img")
    val od = new File(outDir)
    if (!od.exists) od.mkdir
    val alreadyHave = od.list.map(f => FilenameUtils.getBaseName(f))

    // where to get the JSON to extract image URLs from
    val inDir = new File(config.getString("bigOven.json"))
    val recipes = inDir.list
    if (recipes.isEmpty) {
      println("No recipes found. Run BigOvenRecipes to scrape URLs to download.")
      return
    }

    // Extract URLs from JSON and check their validity
    // Determine where to save the files
    val pattern = """(?<="image": ")[^"]+(?=")""".r
    val sourceTarget = for {
      recipeFile <- recipes.par

      recipeID = FilenameUtils.getBaseName(recipeFile)

      // ignore indices outside of user-supplied range
      id = Try(recipeID.toInt).getOrElse(0)
      if id <= endIdx.get && id >= startIdx.get

      // no need to download if we've got it
      if ! alreadyHave.contains(recipeID)

      // can we get a URL?
      recipeSource = scala.io.Source.fromFile(s"${inDir.getAbsoluteFile}/$recipeFile")
      recipe = try{ recipeSource.mkString } finally { recipeSource.close }
      urlLoc = pattern.findFirstIn(recipe)
      if urlLoc.nonEmpty

      // Is the URL valid?
      url = Try(new URL(urlLoc.get))
      if url.isSuccess

      // set up file to write to
      ext = FilenameUtils.getExtension(urlLoc.get)
      targetFileLoc = s"$outDir/$recipeID.$ext"
      targetFile = new File(targetFileLoc)
    } yield url.get -> targetFile

    val pb = new me.tongfei.progressbar.ProgressBar("downloading", 100)
    pb.start()
    pb.maxHint(sourceTarget.length)

    // Now to actually download from the URLs
    sourceTarget.seq.foreach{ case (source, target) =>
      val didItWork = Try(source.#>(target).!!) // the system command to download
      Thread.sleep(delay + r.nextInt(100) + 1)

      // if at first we don't succeed, try try again just once in case the server was just overloaded
      if (didItWork.isFailure) {
        Try(source.#>(target).!!)
        Thread.sleep(delay + r.nextInt(100) + 1)
      }
      pb.step()
    }

    pb.stop()
  }
}