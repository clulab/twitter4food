package org.clulab.twitter4food.util

import com.typesafe.config.ConfigFactory
import org.clulab.twitter4food.struct.TwitterAccount
import org.clulab.twitter4food.util.FileUtils._
import org.slf4j.LoggerFactory
import scala.util.Random.shuffle
import scala.collection.JavaConverters._

/**
  * Split the overweight classifier data into even portions and save the portion assignments in CSV format
  * @author Terron Ishida
  * @author Dane Bell
  */
object SplitData {
  val logger = LoggerFactory.getLogger(this.getClass)

  def main(args: Array[String]) {
    val config = ConfigFactory.load

    val variable = if (args.isEmpty) "overweight" else args.head
    val labels = config
      .getList(s"classifiers.$variable.possibleLabels")
      .unwrapped()
      .asScala
      .toSet
      .asInstanceOf[Set[String]]

    val inputFile = config.getString(s"classifiers.$variable.data")
    val foldsLoc = config.getString(s"classifiers.$variable.folds")
    val usFoldsLoc = config.getString(s"classifiers.$variable.usFolds")

    logger.info(s"Reading in data from ${inputFile}")
    val labeledAccounts = FileUtils.loadTwitterAccounts(inputFile)
      .toSeq
      .filter(_._1.tweets.nonEmpty)

    val wholeSplits = split(labeledAccounts, labels)
    writeToCsv(foldsLoc, wholeSplits)

//    // Scale number of accounts so that sample corresponds to US proportion of overweight
//    val usProps = Map( "Overweight" -> 0.6976, "Not overweight" -> 0.3024 ) // real stats on overweight in US from CDC
//    val usSample = Utils.subsample(labeledAccounts, usProps)
//
//    val usSplits = split(usSample)
//    writeToCsv(usFoldsLoc, usSplits)
  }

  def split(sample: Seq[(TwitterAccount, String)],
    possibleLabels: Set[String],
    numFolds: Int = 10): Seq[Seq[String]] = {

    val (pos, neg) = shuffle(sample)
      .filter{ case (acct, lbl) => possibleLabels.contains(lbl) }
      .partition{ case (acct, lbl) => lbl == "Overweight" }

    println(s"overweight.size=${pos.size}")
    println(s"notOverweight.size=${neg.size}")
    println(s"other=${sample.size - (pos.size + neg.size)}")

    val oneStep = 1.0 / numFolds
    val allSamples = for {
      portion <- 0 until numFolds
    } yield {
      val posStart = (pos.length * portion * oneStep).floor.toInt
      val posLast = if (portion + 1 == numFolds)
        pos.length
      else
        (pos.length * (portion + 1) * oneStep).floor.toInt
      val posSample = pos.slice(posStart, posLast).map { case (account, label) =>
        Seq(portion.toString, account.id.toString, label)
      }

      val negStart = (neg.length * portion * oneStep).floor.toInt
      val negLast = if (portion + 1 == numFolds)
        neg.length
      else
        (neg.length * (portion + 1) * oneStep).floor.toInt
      val negSample = neg.slice(negStart, negLast).map { case (account, label) =>
        Seq(portion.toString, account.id.toString, label)
      }

      posSample ++ negSample
    }

    allSamples.flatten
  }
}
