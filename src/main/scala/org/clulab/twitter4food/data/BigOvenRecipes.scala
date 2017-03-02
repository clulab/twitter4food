package org.clulab.twitter4food.data

import java.io._
import java.net.URL

import com.typesafe.config.{Config, ConfigFactory}
import jline.console.ConsoleReader
import org.slf4j.{Logger, LoggerFactory}

import scala.util.Try

/**
  * Retrieve recipes from BigOven in a JSON format similar to https://schema.org/Recipe
  */
object BigOvenRecipes {

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

    val costPerRecipe = (delay + (perturbAmount / 2.0)) / 1000

    var goal: Option[Int] = None
    while (goal.isEmpty) {
      println(f"How many recipes to fetch (each will take > $costPerRecipe%1.1f s)")
      reader.readLine match {
        case num if Try(num.toInt).isSuccess => goal = Option(num.toInt)
        case other => ()
      }
    }
    logger.info(s"Fetching ${goal.get} recipes")

    var startIdx: Option[Int] = None
    while (startIdx.isEmpty) {
      println("Skip to index")
      reader.readLine match {
        case num if Try(num.toInt).isSuccess =>
          val candidate = num.toInt
          if (candidate < 1) startIdx = Option(1)
          else if (candidate > maxIdx) startIdx = Option(maxIdx)
          else startIdx = Option(candidate)
        case other => startIdx = Option(1)
      }
    }
    if(startIdx.get > 1) logger.info(s"Skipping to index ${startIdx.get}")

    val pb = new me.tongfei.progressbar.ProgressBar("downloading", 100)
    pb.start()
    pb.maxHint(goal.get)

    val outDir = config.getString("bigOven.json")
    val outFile = new File(outDir)
    if (!outFile.exists()) outFile.mkdir

    var i = startIdx.get
    var retrieved = 0
    while (i <= maxIdx && retrieved < goal.get) {
      // try to get JSON for this ID
      val urlText = s"https://www.bigoven.com/recipe/$i"
      val json = getJsonFromUrl(urlText)

      // if we find one, save it to its own file
      if (json.nonEmpty) {
        val jFile = new File(s"$outDir/$i.txt")
        val jWriter = new BufferedWriter(new FileWriter(jFile, false))

        // add ID to JSON itself as well (no longer follows schema.org's recipe definition)
        val writeable = s"""{\n  "id": $i,\n${json.get}}\n"""
        jWriter.write(writeable)
        jWriter.close()

        // successfully got one, so iterate number retrieved
        retrieved += 1
        pb.step()
      }

      // Must sleep whether we got good results or not
      Thread.sleep(delay + r.nextInt(perturbAmount) + 1)
      i += 1
    }

    pb.stop()

    logger.info(s"Last index: $i")
    logger.info(s"$retrieved recipes retrieved")
  }

  def getJsonFromUrl(urlText: String): Option[String] = {
    // Make sure the URL is valid
    val url = Try(new URL(urlText))
    if (url.isFailure) return None

    // Slurp the URL text
    val page = Try(new BufferedReader(new InputStreamReader(url.get.openStream())).lines.toArray.mkString("\n"))
    if (page.isFailure) return None

    // Find JSON data
    val pattern = "(?s)(?<=\"@type\": \"Recipe\",\n)(.+)(?=}\n</script>)".r
    val json = pattern.findFirstIn(page.get)
    if (json.isEmpty) return None

    // Make sure we have a valid image
    val default = "https://photos.bigoven.com/recipe/hero/recipe-no-image.jpg".r
    if (default.findFirstIn(json.get).isEmpty) json else None
  }
}