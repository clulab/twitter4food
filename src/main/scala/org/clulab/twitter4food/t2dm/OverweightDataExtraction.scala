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

        val writer = new PrintWriter("src/main/resources/org/clulab/twitter4food/featureclassifier/overweight/overweightDataVerbose.txt")

        val api = new TwitterAPI(3)

        var i = 0
        for (line <- Source.fromFile("src/main/resources/org/clulab/twitter4food/featureclassifier/overweight/overweightData.txt").getLines){
            // Parse line
            val tuple = line.split("\t")
            val handle = tuple(0).substring(1) // ignore beginning '@'
            val classification = tuple(1)

            var account: TwitterAccount = null
            try {
                account = api.fetchAccount(handle, false, false)
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
