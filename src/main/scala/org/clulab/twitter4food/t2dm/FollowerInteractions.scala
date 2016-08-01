package org.clulab.twitter4food.t2dm

import java.io.PrintWriter

import org.clulab.twitter4food.twitter4j.TwitterAPI

/**
  * Created by Terron on 7/31/16.
  */
object FollowerInteractions {
  def main(args: Array[String]): Unit = {

    val config = com.typesafe.config.ConfigFactory.load
    var followersFile = scala.io.Source.fromFile(config.getString("classifiers.features.followerRelations"))
    val api = new TwitterAPI(1)

    // Set progress bar
    val pb = new me.tongfei.progressbar.ProgressBar("FollowerInteractions", 100)
    pb.start()
    pb.maxHint(followersFile.getLines.length)
    pb.setExtraMessage("Getting follower interactions...")

    followersFile.close
    followersFile = scala.io.Source.fromFile(config.getString("classifiers.features.followerRelations"))

    val outputFile = "followerInteractions.txt" // to be moved to config's specified folder
    val output = new PrintWriter(outputFile)

    for (line <- followersFile.getLines) {
      val handles = line.split("\t")
      val main = handles.head
      var followers = handles.tail

      val mainAccount = api.fetchAccount(main, fetchTweets = false, fetchNetwork = false)

      var lineToWrite = main
      var interactions = List[String]()

      if (mainAccount != null) {
        val candidates = api.search(Array(main))
        for (candidate <- candidates.toSeq.sortWith(_._2.size > _._2.size).map(_._1)) {
          if (followers contains candidate) {
            interactions = candidate :: interactions
            interactions = candidates(candidate).length.toString :: interactions
            followers = followers.filter(f => !(f.equals(candidate)))
          }
        }
        for (follower <- followers) {
          interactions = follower :: interactions
          interactions = "0" :: interactions
        }
      }
      
      lineToWrite += "\t" + interactions.mkString("\t") + "\n"
      output.write(lineToWrite)
      pb.step
    }

    pb.stop

    followersFile.close
    output.close
  }
}
