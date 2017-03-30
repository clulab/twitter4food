package org.clulab.twitter4food.util

import com.typesafe.config.ConfigFactory
import org.clulab.twitter4food.struct.TwitterAccount
import org.clulab.twitter4food.util.FileUtils._
import org.slf4j.LoggerFactory
import scala.util.Random.shuffle

/**
  * Split the overweight classifier data into even portions and save the portion assignments in CSV format
  * @author Terron Ishida
  * @author Dane Bell
  */
object SplitData {
  val logger = LoggerFactory.getLogger(this.getClass)

  def main(args: Array[String]) {
    val config = ConfigFactory.load

    val inputFile = config.getString("classifiers.overweight.data")
    val foldsLoc = config.getString("classifiers.overweight.folds")
    val usFoldsLoc = config.getString("classifiers.overweight.usFolds")

    logger.info(s"Reading in data from ${inputFile}")
    val labeledAccounts = FileUtils.load(inputFile)
      .toSeq
      .filter(_._1.tweets.nonEmpty)

    val wholeSplits = split(labeledAccounts)
    writeToCsv(foldsLoc, wholeSplits)

    // Scale number of accounts so that sample corresponds to US proportion of overweight
    val usProps = Map( "Overweight" -> 0.6976, "Not overweight" -> 0.3024 ) // real stats on overweight in US from CDC
    val usSample = Utils.subsample(labeledAccounts, usProps)

    val usSplits = split(usSample)
    writeToCsv(usFoldsLoc, usSplits)
  }

  def split(sample: Seq[(TwitterAccount, String)], numFolds: Int = 10): Seq[Seq[String]] = {
    val acceptableLabels = Set("Overweight", "Not overweight")

    val (overweight, notOverweight) = shuffle(sample)
      .filter{ case (acct, lbl) => acceptableLabels.contains(lbl) }
      .partition{ case (acct, lbl) => lbl == "Overweight" }

    println(s"overweight.size=${overweight.size}")
    println(s"notOverweight.size=${notOverweight.size}")
    println(s"other=${sample.size - (overweight.size + notOverweight.size)}")

    val oneStep = 1.0 / numFolds
    val allSamples = for {
      portion <- 0 until numFolds
    } yield {
      val owStart = (overweight.length * portion * oneStep).floor.toInt
      val owLast = if (portion + 1 == numFolds)
        overweight.length
      else
        (overweight.length * (portion + 1) * oneStep).floor.toInt
      val owSample = overweight.slice(owStart, owLast).map { case (account, label) =>
        Seq(portion.toString, account.id.toString, label)
      }

      val noStart = (notOverweight.length * portion * oneStep).floor.toInt
      val noLast = if (portion + 1 == numFolds)
        notOverweight.length
      else
        (notOverweight.length * (portion + 1) * oneStep).floor.toInt
      val noSample = notOverweight.slice(noStart, noLast).map { case (account, label) =>
        Seq(portion.toString, account.id.toString, label)
      }

      owSample ++ noSample
    }

    allSamples.flatten
  }
}
