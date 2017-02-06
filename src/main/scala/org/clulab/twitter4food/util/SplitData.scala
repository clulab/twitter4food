package org.clulab.twitter4food.util

import com.typesafe.config.ConfigFactory

/**
  * Created by Terron on 3/28/16.
  */
object SplitData {
  def main(args: Array[String]) {
    val config = ConfigFactory.load

    val inputFile = config.getString("classifiers.overweight.data")

    val trainingFile = config.getString("classifiers.overweight.trainingData")
    val devFile = config.getString("classifiers.overweight.devData")
    val testFile = config.getString("classifiers.overweight.testData")

    println(s"Reading in data from ${inputFile}")
    val labeledAccounts = FileUtils.load(inputFile)
      .toSeq
      .filter(_._1.tweets.nonEmpty)

    // Scale number of accounts so that weights aren't too biased against Overweight
    val desiredProps = Map( "Overweight" -> 0.5, "Not overweight" -> 0.5 )
    val subsampled = Utils.subsample(labeledAccounts, desiredProps)

    val acceptableLabels = Set("Overweight", "Not overweight")
    val (overweight, notOverweight) = subsampled
      .filter{ case (acct, lbl) => acceptableLabels.contains(lbl) }
      .partition{ case (acct, lbl) => lbl == "Overweight" }

    println(s"overweight.size=${overweight.size}")
    println(s"notOverweight.size=${notOverweight.size}")
    println(s"other=${labeledAccounts.size - (overweight.size + notOverweight.size)}")

    val percentTraining = 0.6
    val percentDev = 0.2
    // percentTest falls from the previous two

    // Find limits
    val overweightLim1 = (percentTraining * overweight.size).toInt
    val overweightLim2 = ((percentTraining + percentDev) * overweight.size).toInt

    val notOverweightLim1 = (percentTraining * notOverweight.size).toInt
    val notOverweightLim2 = ((percentTraining + percentDev) * notOverweight.size).toInt

    // Slice data
    val trainingOverweight = overweight.slice(0, overweightLim1)
    val devOverweight = overweight.slice(overweightLim1, overweightLim2)
    val testOverweight = overweight.slice(overweightLim2, overweight.size)

    val trainingNotOverweight = notOverweight.slice(0, notOverweightLim1)
    val devNotOverweight = notOverweight.slice(notOverweightLim1, notOverweightLim2)
    val testNotOverweight = notOverweight.slice(notOverweightLim2, notOverweight.size)

    // Combine
    val trainingSet = (trainingOverweight ++ trainingNotOverweight).toMap
    val devSet = (devOverweight ++ devNotOverweight).toMap
    val testSet = (testOverweight ++ testNotOverweight).toMap

    println(s"trainingSet.size=${trainingSet.size}")
    println(s"devSet.size=${devSet.size}")
    println(s"testSet.size=${testSet.size}")

    // Write to file
    FileUtils.saveToFile(trainingSet.keys.toList, trainingSet.values.toList, trainingFile)
    FileUtils.saveToFile(devSet.keys.toList, devSet.values.toList, devFile)
    FileUtils.saveToFile(testSet.keys.toList, testSet.values.toList, testFile)
  }
}
