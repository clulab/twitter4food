/*
 * Terron Ishihara
 *
 * A script to determine which users in the database are most relevant.
 * Relevance here is defined by
 * - users who most frequently tweet (who have the most tweets in the database)
 * - have a picture (TODO: consider using only pics that contain the user's face)
 * - being human (not an organization or blog)
 * - being likely overweight
 */

import java.io.PrintWriter

import scala.io.Source
import scala.collection.mutable

object RelevantUsers {
    def main(args: Array[String]) {
        // Note the location of the .txt file (not necessarily included on github)
        val databasePath = "/Users/Terron/Documents/twitter4food/food_sample_2Oct2013_7Jan2016.txt"
        var userTweetCounts = new mutable.HashMap[String, Int]().withDefaultValue(0)

        // Iterate over text file. Tweets are represented by three lines
        // 1. tab-separated info about user
        //    handle, name, id, location, followers count, utc offset, time zone, creation timestamp, language
        // 2. tab-separated info about tweet
        //    creation timestamp if available, location coordinates if available, place name if available
        // 3. the tweet itself
        println("=== Iterating over database of tweets...")

        // The maximum heap size may have to be increased to process the entire file
        for ((line, index) <- Source.fromFile(databasePath).getLines.zipWithIndex) {
            if (index % 3 == 0) {
                // First term on first line is the twitter handle
                val twitterHandle = line.splitAt(line.indexOf('\t'))._1
                userTweetCounts(twitterHandle) += 1

                // Debug print to view progress
                if (index > 10000 && index % 10000 == 0) {
                    val count = userTweetCounts(twitterHandle)
                    println(s"$index\t$twitterHandle\t$count")
                }
            }
        }

        // Write out sorted users to file
        // NOTE: Local file path used since I ran this with the scala command, not by building the project
        var writer = new PrintWriter("/Users/Terron/Documents/Git/twitter4food/src/main/resources/org.clulab.twitter4food.t2dm/usersSortedByTweetCounts.txt")

        println("\n=== Users sorted by highest food-related tweet counts:")
        // Print first few just for viewing purposes
        var numUsersToPrint = 200
        // Sort by highest food-related tweet count first
        for ((handle, count) <- userTweetCounts.toSeq.sortBy(_._2).reverse) {
            if (numUsersToPrint > 0) {
                println(s"$handle\t$count")
                numUsersToPrint -= 1
            }
            writer.write(handle + "\t" + count + "\n")
        }
        writer.close

        writer = new PrintWriter("/Users/Terron/Documents/Git/twitter4food/src/main/resources/org.clulab.twitter4food.t2dm/usersMidrange.txt")
        val minimum = 10
        val maximum = 7000

        System.out.println("Total number of users: " + userTweetCounts.size)
        // Filter in users whose counts are in the middle range of tweet counts
        val usersMidrange = userTweetCounts.filter(tup => tup._2 >= minimum && tup._2 <= maximum)
        System.out.println("Number of users in range [" + minimum + ", " + maximum + "]: " + usersMidrange.size)

        // Sort by highest food-related tweet count first
        for ((handle, count) <- usersMidrange.toSeq.sortBy(_._2).reverse) {
            if (numUsersToPrint > 0) {
                println(s"$handle\t$count")
                numUsersToPrint -= 1
            }
            writer.write(handle + "\t" + count + "\n")
        }
        writer.close

    }
}