package org.clulab.twitter4food.data

import java.io.{BufferedWriter, FileWriter}
import java.nio.file.{Files, Paths}

import org.clulab.twitter4food.twitter4j.TwitterAPI

object Followees {

  def main(args: Array[String]) {

    if (args.length < 1) {
      println("Usage: ...Followees <API key number> <classifier variable>")
      return
    }
    val numProcesses = 18
    val keySet = args(0).toInt
    assert(numProcesses > keySet, "API key number exceeds number of API keys available!")

    val dataSet = if (args.length > 1) args(1) else "overweight"
    if (!Set("overweight", "gender", "human").contains(dataSet)) {
      println(s"$dataSet not recognized. Choose from (overweight, gender, human)")
      return
    }

    val config = com.typesafe.config.ConfigFactory.load
    // Input
    var inputFile = scala.io.Source.fromFile(config.getString(s"classifiers.$dataSet.newHandles"))
    val numLines = inputFile.getLines.length
    inputFile.close()
    inputFile = scala.io.Source.fromFile(config.getString(s"classifiers.$dataSet.newHandles"))
    val lines = inputFile.getLines

    // Output
    val relationsFile = config.getString(s"classifiers.$dataSet.newFolloweeRelations") + keySet + ".txt"
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
        writer.write(s"""${followees.getOrElse(Nil).mkString("\t")}\n""")
      }
      pb.step
    }
    pb.stop

    writer.close()
  }

}