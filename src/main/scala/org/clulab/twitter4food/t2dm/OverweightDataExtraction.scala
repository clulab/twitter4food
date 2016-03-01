package org.clulab.twitter4food.t2dm

import java.io.PrintWriter
import java.text.SimpleDateFormat

import org.clulab.twitter4food.struct.{Tweet, TwitterAccount}
import org.clulab.twitter4food.twitter4j.TwitterAPI
import twitter4j.TwitterException

import scala.io.Source

/**
  * Created by Terron on 2/17/16.
  */
object OverweightDataExtraction {
    def main(args: Array[String]) {

        val numProcesses = 16

        // Parse keySet value from args
        var keySet: Int = -1
        if (args.length == 1) {
            keySet = args(0).toInt
        } else {
            println("Usage: OverweightDataExtraction [keySetValue]")
            System.exit(1)
        }

        // Check to make sure keySet is valid
        if (keySet < 0 || keySet > numProcesses-1) {
            println(s"keySet must be in range [0,${numProcesses-1}]")
            System.exit(1)
        }

        println("OverweightDataExtraction: Instantiating PrintWriter...")
        val writer = new PrintWriter("src/main/resources/org/clulab/twitter4food/featureclassifier/overweight/overweightData_" + keySet + ".txt")

        println("OverweightDataExtraction: Creating instance of TwitterAPI...")
        val api = new TwitterAPI(keySet, isAppOnly=true)

        println("OverweightDataExtraction: Calculating total number of accounts...")
        // Find the total number of lines to parse
        var numLines = 0
        for (line <- Source.fromFile("src/main/resources/org/clulab/twitter4food/featureclassifier/overweight/overweightLabels.txt").getLines)
            numLines += 1

        val window = numLines / numProcesses

        println("OverweightDataExtraction: Iterating over accounts...")
        var i = 0
        for (line <- Source.fromFile("src/main/resources/org/clulab/twitter4food/featureclassifier/overweight/overweightLabels.txt").getLines) {
            // Only process the lines in this window
            if ( i >= keySet * window && i < (keySet + 1) * window ) {
                // Parse line
                val tuple = line.split("\t")
                val handle = tuple(0).substring(1) // ignore beginning '@'
                val classification = tuple(1)

                var account: TwitterAccount = null
                try {
                    account = api.fetchAccount(handle, true, false) // fetchTweets is true
                } catch {
                    case te: TwitterException => // ignore suspended accounts
                }
                // Only include accounts that are in English
                if (account != null && (account.lang equals "en")) {
                    writer.write(s"${classification}\n")
                    writer.write(s"${account.handle}\t${account.id}\t${account.name.replace("\n"," ")}\t${account.lang}\t${account.location}\n")
                    writer.write(s"${account.description.replace("\n"," ")}\n")
                    account.tweets.foreach(tweet =>
                        if (tweet.lang equals "en")
                            writer.write(s"${tweet.id}\t${tweet.createdAt}\t${tweet.text.replace("\n"," ")}\n"))
                }

                println(s"Processed line ${i}, handle @${handle}")
            }

            i += 1
        }

        println("\n\nOverweightDataExtraction: Finished!")

        writer.close
    }

    def parse(filePath: String): Map[TwitterAccount, String] = {
        // account and label to be mapped together
        var classification = ""

        // Data to fill account
        var accountInfo: Array[String]  = null
        var description = ""
        var tweets = Seq[Tweet]()

        // Can only add account once first account has been processed
        var isFirst = true

        // The result map to be returned
        var result = Map[TwitterAccount, String]()

        for (line <- Source.fromFile(filePath).getLines){
            var split = line.split("\t")

            // Classification falls on its own line
            if ((line equals "Overweight") || (line equals "Not overweight") || (line equals "Can't tell")) {
                if (!isFirst) {
                    // Only add accounts that are human
                    if (!(classification equals "Can't tell")) {
                        val account = new TwitterAccount(accountInfo(0), accountInfo(1).toLong, accountInfo(2), accountInfo(3), null, accountInfo(4), description, tweets)
                        result = result + (account -> classification)
                    }

                    accountInfo = null
                    description = ""
                    tweets = Seq()
                }
                else {
                    isFirst = false
                }

                classification = line
            }
            // If second item is a long (the account's id), line is account info
            else if (split.length > 1 && split(1).forall(c => c.isDigit)) {
                while (split.length < 5) {
                    split = split :+ ""
                }
                accountInfo = split
            }
            // If first item is a long (the tweet's id), line is tweet info
            else if (split.length > 1 && split(0).forall(c => c.isDigit)) {
                val df = new SimpleDateFormat("EEE MMM dd HH:mm:ss zzz yyyy")
                val date = df.parse(split(1))

                if (tweets.length < 50)
                    tweets = tweets :+ new Tweet(split(2), split(0).toLong, null, date, accountInfo(0))
            }
            // Otherwise line must be description text (no discrete format to check here, so handling it with else
            else {
                description = line
            }
        }

        // Add last account that wasn't processed yet
        if (!(classification equals "Can't tell")) {
            val account = new TwitterAccount(accountInfo(0), accountInfo(1).toLong, accountInfo(2), accountInfo(3), null, accountInfo(4), description, tweets)
            result = result + (account -> classification)
        }

        result
    }
}
