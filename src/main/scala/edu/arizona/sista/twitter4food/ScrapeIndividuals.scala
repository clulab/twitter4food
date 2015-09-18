package edu.arizona.sista.twitter4food

import java.io.{FileOutputStream, PrintWriter, File}

import twitter4j._
import twitter4j.conf.ConfigurationBuilder

import java.util.Date
import scala.collection.JavaConversions.asScalaBuffer

object ScrapeIndividuals {
  val SLEEP = 3100

  def dateToString(d: Date): String = if(d == null) "NIL" else d.toString

  def placeToString(p: Place): String = {
    if (p == null) return "NIL"
    val os = new StringBuilder
    os.append(c(p.getPlaceType()))
    os.append("/" + c(p.getFullName()))
    os.append("/" + c(p.getCountryCode()))
    os.append("/" + c(p.getBoundingBoxType()))
    val gs = p.getBoundingBoxCoordinates
    if (gs != null) {
      for {
        i <- 1 until gs.length
        j <- 1 until gs(i).length
      } {
        os.append("/" + geoLocationToString(gs(i)(j)))
      }
    }
    os.toString
  }

  def geoLocationToString(g: GeoLocation): String = {
    if(g == null) return "NIL"
    c(g.getLatitude.toString) + "|" + c(g.getLongitude.toString)
  }

  def c(s: String): String = {
    if(s == null) return "NIL"
    if(s.length() == 0) return "NIL"
    s.replaceAll("[\\t\\n\\r]+", " ")
  }

  def main(args: Array[String]) = {
    val ratedIndividualsFiles = "/work/dane/rated_individuals.csv"
    val bufferedSource = scala.io.Source.fromFile(ratedIndividualsFiles))
    val userWeight = (for (line <- bufferedSource.getLines) yield {
      val Array(userHandle, label) = line.split(",").map(_.trim)
      userHandle -> label
    }).toMap
    bufferedSource.close
    val userHandles = userWeight.keys
    var number = 0

    for(userHandle <- userHandles){
      number += 1
      val weight = userWeight(userHandle)
      println("userHandle=" + userHandle + " weight=" + weight)
      println("number=" + number)

      val directoryRoot = "/data/nlp/corpora/twitter4food/ratedIndividuals/"
      val fileRoot = new File(directoryRoot)
      if(!fileRoot.exists()){
        fileRoot.mkdir()
      }
      val directory = directoryRoot + weight
      val file = new File(directory)
      if(!file.exists()){
        file.mkdir()
      }
      val output = directory + "/" + userHandle + ".txt"
      val userfile = new File(output)
      if(!userfile.exists()){
        val pw = new PrintWriter(new FileOutputStream(output, true))
        val cb = new ConfigurationBuilder()
        cb.setDebugEnabled(false)
        // app name: search4food ***** Twitter4Food
        //first

        //      cb.setOAuthConsumerKey("")
        //      cb.setOAuthConsumerSecret("")
        //      cb.setOAuthAccessToken("")
        //      cb.setOAuthAccessTokenSecret("")

        //fail
        //      cb.setOAuthConsumerKey("")
        //      cb.setOAuthConsumerSecret("")
        //      cb.setOAuthAccessToken("")
        //      cb.setOAuthAccessTokenSecret("")


        //    cb.setOAuthConsumerKey("")
        //    cb.setOAuthConsumerSecret("")
        //    cb.setOAuthAccessToken("")
        //    cb.setOAuthAccessTokenSecret("")

        //
        //    cb.setOAuthConsumerKey("")
        //    cb.setOAuthConsumerSecret("")
        //    cb.setOAuthAccessToken("")
        //    cb.setOAuthAccessTokenSecret("")

        //
        //      cb.setOAuthConsumerKey("")
        //      cb.setOAuthConsumerSecret("")
        //      cb.setOAuthAccessToken("")
        //      cb.setOAuthAccessTokenSecret("")


        //       app name: search4food2
        //      cb.setOAuthConsumerKey("")
        //      cb.setOAuthConsumerSecret("")
        //      cb.setOAuthAccessToken("")
        //      cb.setOAuthAccessTokenSecret("")


        //for DELL computer
        //	       cb.setOAuthConsumerKey("")
        //   cb.setOAuthConsumerSecret("")
        //   cb.setOAuthAccessToken("")
        //   cb.setOAuthAccessTokenSecret("")
        ////
        //


        //   xps 0
        cb.setOAuthConsumerKey("")
        cb.setOAuthConsumerSecret("")
        cb.setOAuthAccessToken("")
        cb.setOAuthAccessTokenSecret("")


        //xps 1
        //	         cb.setOAuthConsumerKey("")
        //   cb.setOAuthConsumerSecret("")
        //   cb.setOAuthAccessToken("")
        //   cb.setOAuthAccessTokenSecret("")
        //
        ////


        //xps 2
        //	  	         cb.setOAuthConsumerKey("")
        //   cb.setOAuthConsumerSecret("")
        //   cb.setOAuthAccessToken("")
        //   cb.setOAuthAccessTokenSecret("")
        //


        //xps3
        //	  	  	         cb.setOAuthConsumerKey("")
        //   cb.setOAuthConsumerSecret("")
        //   cb.setOAuthAccessToken("")
        //   cb.setOAuthAccessTokenSecret("")


        //   xps 4
        //       cb.setOAuthConsumerKey("")
        //cb.setOAuthConsumerSecret("")
        //cb.setOAuthAccessToken("")
        //cb.setOAuthAccessTokenSecret("")


        //   xps 5
        //      cb.setOAuthConsumerKey("")
        //cb.setOAuthConsumerSecret("")
        //cb.setOAuthAccessToken("")
        //cb.setOAuthAccessTokenSecret("")


        //   xps 6
        //      cb.setOAuthConsumerKey("")
        //cb.setOAuthConsumerSecret("")
        //cb.setOAuthAccessToken("")
        //cb.setOAuthAccessTokenSecret("")


        //   xps 7
        //   cb.setOAuthConsumerKey("")
        //cb.setOAuthConsumerSecret("")
        //cb.setOAuthAccessToken("")
        //cb.setOAuthAccessTokenSecret("")


        //   xps 8
        //      cb.setOAuthConsumerKey("")
        //   cb.setOAuthConsumerSecret("")
        //   cb.setOAuthAccessToken("")
        //   cb.setOAuthAccessTokenSecret("")

        //   xps 9
        //    	      cb.setOAuthConsumerKey("")
        //    	   cb.setOAuthConsumerSecret("")
        //    	   cb.setOAuthAccessToken("")
        //    	   cb.setOAuthAccessTokenSecret("")



        //  	   //   xps 10
        //     	      cb.setOAuthConsumerKey("")
        //     	   cb.setOAuthConsumerSecret("")
        //     	   cb.setOAuthAccessToken("")
        //     	   cb.setOAuthAccessTokenSecret("")
        //


        println("Getting Tweets: ")

        ////////////////Right here the program really starts - Gets the user tweets////

        val twitter: Twitter = new TwitterFactory(cb.build()).getInstance()
        for (i <- 1 to 16) {//get the first i pages of 200 tweets (we expect i*200 tweets), max 3200 total
        val paging = new Paging(i, 200); //200 is the max # of tweets per page
        val statuses = twitter.getUserTimeline(userHandle, paging)
          if(statuses!=null){
            val u = statuses.get(0).getUser()
            val uCreatedAt = dateToString(u.getCreatedAt())
            for (status <- asScalaBuffer(statuses)) {
              pw.println(
                "@" + u.getScreenName() + "\t" +
                  c(u.getName()) + "\t" +
                  c(u.getId().toString) + "\t" +
                  c(u.getLocation()) + "\t" +
                  c(u.getFollowersCount().toString) + "\t" +
                  c(u.getUtcOffset().toString) + "\t" +
                  c(u.getTimeZone()) + "\t" +
                  c(uCreatedAt.toString) + "\t" +
                  c(u.getLang()) + "\n" +
                  c(dateToString(status.getCreatedAt())) + "\t" +
                  geoLocationToString(status.getGeoLocation()) + "\t" +
                  placeToString(status.getPlace()) + "\n" +
                  c(status.getText()))
              pw.flush()
            }
          }
          Thread.sleep(SLEEP)
        }
      }
    }
  }
}


