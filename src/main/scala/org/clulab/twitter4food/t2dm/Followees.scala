package org.clulab.twitter4food.t2dm

import java.io.{BufferedWriter, FileWriter}
import org.clulab.twitter4food.twitter4j.TwitterAPI

object Followees {

  def main(args: Array[String]) {

    val numProcesses = 18
    val keySet = args(0).toInt

    val config = com.typesafe.config.ConfigFactory.load
    // Input
    var inputFile = scala.io.Source.fromFile(config.getString("classifiers.overweight.handles"))
    val numLines = inputFile.getLines.length
    inputFile.close()
    inputFile = scala.io.Source.fromFile(config.getString("classifiers.overweight.handles"))
    val lines = inputFile.getLines
    // Output
    val relationsFile = config.getString("classifiers.features.followeeRelations")

    println(s"Will write relations to $relationsFile")

    // Get account handles
    var handles:Seq[String] = Seq()
    while(lines.hasNext) {
      val line = lines.next
      val elements = line.split("\t")
      handles += elements(0).substring(1) // remove @ symbol
    }

    inputFile.close()

    val window = numLines / (numProcesses - 1)

    val handleItr = handles.grouped(window).toSeq

    val api = new TwitterAPI(keySet)
    val toFetch = handleItr(keySet)

    // Set progress bar
    val pb = new me.tongfei.progressbar.ProgressBar("Followees", 100)
    pb.start()
    pb.maxHint(toFetch.length)
    pb.setExtraMessage(s"keySet=$keySet")

    val relations = for (follower <- toFetch.seq) yield {
      println(s"Fetching $follower's followees...")
      val followees = api.fetchFolloweeHandles(follower)
      Thread.sleep(60100) // 15 accesses per 15-min period => ~60k ms / access
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