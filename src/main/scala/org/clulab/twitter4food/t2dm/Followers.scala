package org.clulab.twitter4food.t2dm

import java.io.{BufferedWriter, FileWriter}

import org.clulab.twitter4food.struct.TwitterAccount
import org.clulab.twitter4food.twitter4j.TwitterAPI
import org.clulab.twitter4food.util.{FileUtils, MultiThreadLoader, TestUtils}
import twitter4j.TwitterException

/**
  * Created by Terron on 4/6/16.
  *
  * A script for downloading the tweets of active followers of the account in our database.
  */
object Followers {

    def main(args: Array[String]) {

        val numProcesses = 16
        val keySet = args(0).toInt

        val config = com.typesafe.config.ConfigFactory.load
        // Input
        var inputFile = scala.io.Source.fromFile(config.getString("classifiers.overweight.handles"))
        val numLines = inputFile.getLines.length
        inputFile.close()
        inputFile = scala.io.Source.fromFile(config.getString("classifiers.overweight.handles"))

        // Output
        val relationsFile = config.getString("classifiers.features.followerRelations") + keySet + ".txt"
        val accountsFile = config.getString("classifiers.features.followerAccounts") + keySet + ".txt"

        println(s"Will write relations to ${relationsFile}")
        println(s"Will write accounts to ${accountsFile}")

        var handles = Set[String]()

        var numO = 0
        var numN = 0

        // Get account handles
        for (line <- inputFile.getLines) {
            val elements = line.split("\t")
            val handle = elements(0).substring(1) // remove @ symbol
            var label = elements(1)
            if ((label equals "OW") || (label equals "OW*"))
                label = "Overweight"
            else if ((label equals "NO") || (label equals "NO*"))
                label = "Not overweight"

            if (!(label equals "Can't tell")) {
                handles += handle
                if ((label equals "Overweight"))
                    numO += 1
                else if ((label equals "Not overweight"))
                    numN += 1
            }
        }
        inputFile.close()

        println(s"Accumulated ${handles.size} handles from input file: ${numO} overweight, ${numN} not overweight")


        val window = handles.size / (numProcesses - 1)

        var handleItr = handles.grouped(window)

        assert(handleItr.length == numProcesses)

        handleItr = handles.grouped(window)

        var groupIndex = 0
        for (i <- 0 until keySet) {
            groupIndex += 1
            handleItr.next
        }

        assert(keySet == groupIndex)

        var followers = Set[TwitterAccount]()
        val api = new TwitterAPI(keySet)
        val toFetch = handleItr.next

        // Set progress bar
        val pb = new me.tongfei.progressbar.ProgressBar("Followers", 100)
        pb.start()
        pb.maxHint(toFetch.size)
        pb.setExtraMessage(s"keySet=${keySet}")

        toFetch.foreach(follower => {
            println(s"Fetching ${follower}...")
            val fetchedAccount = api.fetchAccount(h=follower, fetchTweets=false, fetchNetwork=true)
            if (fetchedAccount != null) {
                fetchedAccount.activeFollowers.foreach(f => followers += f)
            }
            pb.step
        } )
        pb.stop

        println()
        println("Downloaded followers!")

        // Write this account and its 4 active followers to separate file
        val writer = new BufferedWriter(new FileWriter(relationsFile, false))
        followers.foreach(account => {
            if (account != null) {
                writer.write(account.handle + "\t");
                writer.write(account.activeFollowers.map(a => a.handle).mkString("\t") + "\n")
            }
        })

        writer.close()
        println("Relations written to file!")

        // Labels are arbitrary for followers, so just use "follower"
        val labels = followers.toSeq.map(_ => "follower")
        
        FileUtils.saveToFile(followers.toSeq, labels, accountsFile)

        println("Follower accounts written to file!")
    }
}
