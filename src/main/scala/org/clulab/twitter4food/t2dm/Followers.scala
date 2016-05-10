package org.clulab.twitter4food.t2dm

import java.io.{BufferedWriter, FileWriter}

import org.clulab.twitter4food.struct.TwitterAccount
import org.clulab.twitter4food.twitter4j.TwitterAPI
import org.clulab.twitter4food.util.{FileUtils, MultiThreadLoader, TestUtils}
import twitter4j.TwitterException

/**
  * Created by Terron on 4/6/16.
  */
object Followers {

    // A script for downloading the tweets of active followers of the account in our database
    def main(args: Array[String]) {

        val numProcesses = 16

        val config = com.typesafe.config.ConfigFactory.load
        // Input
        var inputFile = scala.io.Source.fromFile(config.getString("classifiers.overweight.labels"))
        val numLines = inputFile.getLines.length
        inputFile.close()
        inputFile = scala.io.Source.fromFile(config.getString("classifiers.overweight.labels"))

        val window = numLines / numProcesses

        // Output
        val relationsFile = config.getString("classifiers.features.followerRelations") + ".txt"
        val accountsFile = config.getString("classifiers.features.followerAccounts") + ".txt"

        val writer = new BufferedWriter(new FileWriter(relationsFile, false))
        var handles = Set[String]()

        // Get account handles
        for (line <- inputFile.getLines) {
            val elements = line.split("\t")
            val handle = elements(0).substring(1) // remove @ symbol
            val label = elements(1)

            if (!(label equals "Can't tell")) {
                handles += handle
            }
        }
        inputFile.close()

        println("Accumulated handles from input file!")

        // NOTE: Keep in mind the order of the accounts is not stable after going through MultiThreadLoader
        // That is, if there are specific labels associated with these accounts, you'll have to remap them afterward
        val accounts = new MultiThreadLoader(handles.toSeq, isAppOnly = true, fetchTweets = false, fetchNetwork = true, poolSize = numProcesses).call

        println("Downloaded followers!")

        // Write this account and its 4 active followers to separate file
        accounts.foreach(account => {
            writer.write(account.handle + "\t");
            writer.write(account.activeFollowers.map(a => a.handle).mkString("\t") + "\n")
        })

        writer.close()
        println("Relations written to file!")

        // Labels are arbitrary for followers, so just use "follower"
        val labels = accounts.toList.map(_ => "follower")
        
        FileUtils.saveToFile(accounts, labels, accountsFile)

        println("Follower accounts written to file!")
    }
}
