package org.clulab.twitter4food.t2dm

import java.io.{BufferedWriter, FileWriter}
import org.clulab.twitter4food.twitter4j.TwitterAPI

object Followees {

  def main(args: Array[String]) {

    val numProcesses = 18
    val keySet = args(0).toInt

    val config = com.typesafe.config.ConfigFactory.load
    // Input
    val inputFile = scala.io.Source.fromFile(config.getString("classifiers.overweight.handles"))
    // Output
    val relationsFile = config.getString("classifiers.features.followeeRelations")

    println(s"Will write relations to $relationsFile")

    // Get account handles
    val handles = (for (line <- inputFile.getLines) yield {
      val elements = line.split("\t")
      elements(0).substring(1) // remove @ symbol
    }).toSeq
    inputFile.close()

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

    val api = new TwitterAPI(keySet)
    val toFetch = handleItr.next

    // Set progress bar
    val pb = new me.tongfei.progressbar.ProgressBar("Followees", 100)
    pb.start()
    pb.maxHint(toFetch.size)
    pb.setExtraMessage(s"keySet=$keySet")

    val relations = for (follower <- toFetch) yield {
      println(s"Fetching $follower's followees...")
      val followees = api.fetchFolloweeHandles(follower)
      Thread.sleep(60100) // 15 accesses per 15-min period
      pb.step
      follower -> followees
    }
    pb.stop

    println("\nDownloaded followees")

    // Write this account and its 4 active followers to separate file
    val writer = new BufferedWriter(new FileWriter(relationsFile, false))
    relations.foreach{ case (acct, followers) =>
        writer.write(s"$acct\t")
        writer.write(s"""${followers.mkString("\t")}\n""")
    }

    writer.close()
    println("Relations written to file!")
  }

}