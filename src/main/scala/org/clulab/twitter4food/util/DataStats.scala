package org.clulab.twitter4food.util

import com.typesafe.config.ConfigFactory

object DataStats {
  def main(args: Array[String]): Unit = {
    val dataset = if (args.isEmpty) "overweight" else args.head

    val config = ConfigFactory.load

    val total = FileUtils.load(config.getString(s"classifiers.$dataset.data")).toSeq.filter(_._1.tweets.nonEmpty)
//    val train = FileUtils.load(config.getString(s"classifiers.$dataset.trainingData")).toSeq
//    val dev = FileUtils.load(config.getString(s"classifiers.$dataset.devData")).toSeq
//    val test = FileUtils.load(config.getString(s"classifiers.$dataset.testData")).toSeq
//    val total = train ++ dev ++ test

//    val dsMap = Map("train" -> train, "dev" -> dev, "test" -> test, "total" -> total)

    val dsMap = Map("total" -> total)

    println("partition,label,numAccounts,totalTweets,tweetsPerAccount,totalTokens,tokensPerTweet")
    dsMap.foreach { case (partition, ds) =>
      val byLabel = ds.groupBy(_._2)
      byLabel.foreach { case (lbl, acctsWithLbls) =>
        val (accts, lbls) = acctsWithLbls.unzip
        val numAccts = accts.length
        val totalTweets = accts.map(_.tweets.length).sum
        val tPerAccount = totalTweets.toDouble / numAccts
        val totalTokens = accts.flatMap(_.tweets.map(_.text.split(" +").length)).sum
        val tPerTweet = totalTokens.toDouble / totalTweets
        println(f"$partition,$lbl,$numAccts,$totalTweets,$tPerAccount%1.0f,$totalTokens,$tPerTweet%1.1f")
      }
    }
  }
}