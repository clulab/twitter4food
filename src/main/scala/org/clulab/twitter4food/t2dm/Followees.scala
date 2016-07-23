package org.clulab.twitter4food.t2dm

import java.io.{BufferedWriter, FileWriter}
import java.nio.file.{Files, Paths}

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
    val relationsFile = config.getString("classifiers.features.followeeRelations") + keySet + ".txt"
    val soFar:Set[String] = if (!Files.exists(Paths.get(relationsFile)))
      Set.empty
    else {
      val lines = scala.io.Source.fromFile(relationsFile).getLines
      val firstCol = for (line <- lines) yield line.split("\t")(0)
      firstCol.toSet
    }

    println(s"Will write relations to $relationsFile")

    // Get account handles
    var handles:Seq[String] = Seq()
    while(lines.hasNext) {
      val line = lines.next
      val elements = line.split("\t")
      handles +:= elements(0).substring(1) // remove @ symbol
    }

    inputFile.close()

    val window = numLines / (numProcesses - 1)

    val handleItr = handles.grouped(window).toSeq

    val api = new TwitterAPI(keySet)
    val toFetch = handleItr(keySet) filterNot soFar.contains

    // Set progress bar
    val pb = new me.tongfei.progressbar.ProgressBar("Followees", 100)
    pb.start()
    pb.maxHint(toFetch.length)
    pb.setExtraMessage(s"keySet=$keySet")

    val writer = new BufferedWriter(new FileWriter(relationsFile, false))
    toFetch.seq.foreach { follower =>
      println(s"Fetching $follower's followees...")
      val followees = try {
        Some(api.fetchFolloweeHandles(follower))
      } catch {
        case e:Exception => None
      }
      if (followees.nonEmpty){
        writer.write(s"$follower\t")
        writer.write(s"""${followees.mkString("\t")}\n""")
      }
      pb.step
    }
    pb.stop

    writer.close()
  }

}