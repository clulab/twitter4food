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
        val (api, config) = TestUtils.init(7, true)
        // Input
        val inputFile = config.getString("classifiers.overweight.labels")
        // Output
        val relationsFile = config.getString("classifiers.features.followerRelations")
        val accountsFile = config.getString("classifiers.features.followerAccounts")

        val writer = new BufferedWriter(new FileWriter(relationsFile, false))
        var accounts = Set[TwitterAccount]()

        for (line <- scala.io.Source.fromFile(inputFile).getLines) {
            val elements = line.split("\t")
            val handle = elements(0).substring(1)
            val label = elements(1)

            var account: TwitterAccount = null
            try {
                account = api.fetchAccount(handle, true, false) // fetchTweets is true
            } catch {
                case te: TwitterException => // ignore suspended accounts
            }
            // Only include accounts that are in English
            if (account != null && (account.lang equals "en")) {
                val account = api.fetchAccount(handle, false, true)
                accounts += account
                writer.write(account.handle + "\t")
                writer.write(account.activeFollowers.map(a => a.handle).mkString("\t") + "\n")
            }
        }
        writer.close()

        // Labels are arbitrary for followers, so just use "follower"
        var labels = List[String]()
        for (_ <- 0 until accounts.size) {labels += "follower"}

        FileUtils.saveToFile(accounts.toSeq, labels, accountsFile)
    }
}
