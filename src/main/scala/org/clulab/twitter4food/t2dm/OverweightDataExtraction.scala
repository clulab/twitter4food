package org.clulab.twitter4food.t2dm

import java.io.PrintWriter

import org.clulab.twitter4food.util.{FileUtils, TestUtils}
import java.text.SimpleDateFormat

import org.clulab.twitter4food.struct.{Tweet, TwitterAccount}
import org.clulab.twitter4food.twitter4j.TwitterAPI
import twitter4j.TwitterException

import scala.io.Source

/**
  * Created by Terron on 2/17/16.
  *
  * A script for downloading the tweets of a file of twitter handles.
  * The file is assumed to have the format of
  * [handle]\t[classificationLabel]
  * on each line.
  */
object OverweightDataExtraction {
    def main(args: Array[String]) {

        val (api, config) = TestUtils.init(0)

        val outputFile = config.getString("overweight") + "/extracted.txt"

        val inputFileStr = config.getString("overweight") + "/overweight_accounts.txt"
        val inputFile = scala.io.Source.fromFile(inputFileStr)
        val lines = inputFile.getLines

        val pb = new me.tongfei.progressbar.ProgressBar("main()", 100)
        pb.start()
        pb.maxHint(508)
        pb.setExtraMessage("Reading accounts...")

        var accounts = List[TwitterAccount]()
        var labels = List[String]()

        for (line <- lines) {
            // Parse line
            val tuple = line.split("\t")
            val handle = tuple(0).substring(1) // ignore beginning '@'
            var classification = tuple(1)
            // Generalize classification
            if ((classification equals "OW") || (classification equals "OW*"))
                classification = "Overweight"
            else if ((classification equals "NO") || (classification equals "NO*"))
                classification = "Not overweight"
            else
                classification = null

            // Error checking for labels
            if (classification != null) {
                // Fetch account
                var account: TwitterAccount = null
                try {
                    account = api.fetchAccount(handle, true, false) // fetchTweets is true, fetchNetwork is false
                } catch {
                    case te: TwitterException => // ignore suspended accounts
                }
                // Only include accounts that are in English
                if ((account != null) && (account.lang equals "en")) {
                    accounts = account :: accounts
                    labels = classification :: labels
                }
            }

            pb.step()
        }
        inputFile.close()
        pb.stop()

        println("OverweightDataExtraction: Saving to file...")
        FileUtils.saveToFile(accounts, labels, outputFile)

        println("\n\nOverweightDataExtraction: Finished!")
    }
}
