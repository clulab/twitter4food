package org.clulab.twitter4food.struct

import org.clulab.twitter4food.util.FileUtils

import scala.collection.mutable.Map

/**
  * Created by Terron on 3/28/16.
  */
object SplitData {
    def main(args: Array[String]) {
        val inputFile = "overweightData.txt"

        val trainingFile = "overweightTraining.txt"
        val devFile = "overweightDev.txt"
        val testFile = "overweightTest.txt"

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

    }
}
