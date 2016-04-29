package org.clulab.twitter4food.t2dm

import java.io.{BufferedWriter, FileWriter}

import org.clulab.twitter4food.struct.TwitterAccount
import org.clulab.twitter4food.twitter4j.TwitterAPI
import org.clulab.twitter4food.util.{FileUtils, TestUtils}
import twitter4j.TwitterException

/**
  * Created by Terron on 4/6/16.
  */
object Followers {
    def main(args: Array[String]) {

        val numProcesses = 16

        // Parse keySet value from args
        var keySet: Int = -1
        if (args.length == 1) {
            keySet = args(0).toInt
        } else {
            println("Usage: Followers [keySetValue]")
            System.exit(1)
        }

        // Check to make sure keySet is valid
        if (keySet < 0 || keySet > numProcesses-1) {
            println(s"keySet must be in range [0,${numProcesses-1}]")
            System.exit(1)
        }

        val (api, config) = TestUtils.init(keySet, true)
        // Input
        var inputFile = scala.io.Source.fromFile(config.getString("classifiers.overweight.labels"))
        val numLines = inputFile.getLines.length
        inputFile.close()
        inputFile = scala.io.Source.fromFile(config.getString("classifiers.overweight.labels"))

        val window = numLines / numProcesses

        // Output
        val relationsFile = config.getString("classifiers.features.followerRelations") + "-" + keySet + ".txt"
        val accountsFile = config.getString("classifiers.features.followerAccounts") + "-" + keySet + ".txt"

        val writer = new BufferedWriter(new FileWriter(relationsFile, false))
        var accounts = Set[TwitterAccount]()

        // Set progress bar
        val pb = new me.tongfei.progressbar.ProgressBar("Followers", 100)
        pb.start()
        pb.maxHint(window)
        pb.setExtraMessage("Downloading active followers...")

        var i = 0
        for (line <- inputFile.getLines) {
            if ( i >= keySet * window && i < (keySet + 1) * window ) {
                val elements = line.split("\t")
                val handle = elements(0).substring(1) // remove @ symbol
                val label = elements(1)

                if (!(label equals "Can't tell")) {
                    var account: TwitterAccount = api.fetchAccount(handle, false, true)
                    println(s"account: ${account.toString}")
                    // Only include accounts that are in English
                    if ((account != null) && (account.lang equals "en")) {
                        // Add active followers to list of accounts to be written to file
                        account.activeFollowers.foreach(ta => accounts += ta)
                        // Write this account and its 4 active followers to separate file
                        writer.write(account.handle + "\t")
                        writer.write(account.activeFollowers.map(a => a.handle).mkString("\t") + "\n")

                        println(s"handle: ${account.handle}, activeFollowers: ${account.activeFollowers.map(a => a.handle).mkString(", ")}")
                    }

                    pb.step()
                }
            }

            i += 1
        }
        pb.stop()
        writer.close()
        inputFile.close()

        // Labels are arbitrary for followers, so just use "follower"
        val labels = accounts.toList.map(_ => "follower")
        
        FileUtils.saveToFile(accounts.toSeq, labels, accountsFile)
    }
}
