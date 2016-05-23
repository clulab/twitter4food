package org.clulab.twitter4food.util

import com.typesafe.config.ConfigFactory
import org.clulab.twitter4food.struct.TwitterAccount

import scala.collection.mutable.Map

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
        var overweight = Map[TwitterAccount, String]()
        var notOverweight = Map[TwitterAccount, String]()
        var other = 0

        for ( (account, label) <- labeledAccounts) {
            if (label equals "Overweight") {
                overweight += (account -> label)
            } else if (label equals "Not overweight") {
                notOverweight += (account -> label)
            } else {
                other += 1
            }
        }

        println(s"overweight.size=${overweight.size}")
        println(s"notOverweight.size=${notOverweight.size}")
        println(s"other=${other}")

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
        val trainingSet = trainingOverweight ++ trainingNotOverweight
        val devSet = devOverweight ++ devNotOverweight
        val testSet = testOverweight ++ testNotOverweight

        println(s"trainingSet.size=${trainingSet.size}")
        println(s"devSet.size=${devSet.size}")
        println(s"testSet.size=${testSet.size}")

        // Write to file
        FileUtils.saveToFile(trainingSet.keys.toList, trainingSet.values.toList, trainingFile)
        FileUtils.saveToFile(devSet.keys.toList, devSet.values.toList, devFile)
        FileUtils.saveToFile(testSet.keys.toList, testSet.values.toList, testFile)
    }
}
