package org.clulab.twitter4food.t2dm

import java.io.PrintWriter

import org.clulab.twitter4food.struct.TwitterAccount
import org.clulab.twitter4food.twitter4j.TwitterAPI
import twitter4j.TwitterException

import scala.io.Source

/**
  * Created by Terron on 2/17/16.
  */
object OverweightDataExtraction {
    def main(args: Array[String]) {

        var keySet: Int = null
        if (args.length == 1) {
            keySet = args(0).toInt
            if (keySet < 0 || keySet > 16) {
                println("keySet must be in range [0,15]")
                System.exit(1)
            }
        } else {
            println("Usage: OverweightDataExtraction [keySetValue]")
            System.exit(1)
        }
        if (keySet == null)
            println("Error in parsing keySet value")
            System.exit(1)

        val writer = new PrintWriter("src/main/resources/org/clulab/twitter4food/featureclassifier/overweight/overweightData_"+keySet+".txt")

        val api = new TwitterAPI(keySet)

        var i = 0
        for (line <- Source.fromFile("src/main/resources/org/clulab/twitter4food/featureclassifier/overweight/overweightData.txt").getLines){
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

            if (account != null) {
                writer.write(s"${classification}\n")
                writer.write(s"${account.handle}\t${account.id}\t${account.name}\t${account.lang}\t${account.location}\n")
                writer.write(s"${account.description}\n")
            }

            i += 1

            println(s"Processed line ${i}")
        }

        writer.close
    }
}
