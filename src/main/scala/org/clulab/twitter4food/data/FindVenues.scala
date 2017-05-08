package org.clulab.twitter4food.data

import org.clulab.twitter4food.util.FileUtils._
import org.clulab.twitter4food.struct.{Location, Venue}

import com.google.maps.{GeoApiContext, PlacesApi}
import com.google.maps.model.LatLng
import com.typesafe.config.{Config, ConfigFactory}
import org.slf4j.{Logger, LoggerFactory}
import me.tongfei.progressbar.ProgressBar


/**
  * Annotates set of [[Location]]s generated from tweet geolocation with nearby [[Venue]]s generated with the help of
  * the Google Places API.
  */
object FindVenues extends App {

  val config: Config = ConfigFactory.load
  val logger: Logger = LoggerFactory.getLogger(this.getClass)
  val sleepTime = 600 // 150000 requests / 24 hours => 576 ms per request
  val maxDist = 50 // meters between venues and tweet location

  // Locations before Venue annotation
  val coordsFile = config.getString("classifiers.overweight.tweetCoords")
  val coords = loadLocations(coordsFile)
  // Place to put annotated Locations
  val locFile = config.getString("classifiers.overweight.tweetLocs")

  // Google Places API key
  val apiKey = scala.io.Source.fromFile(config.getString("places_key")).getLines().next().stripLineEnd
  val context = new GeoApiContext().setApiKey(apiKey)

  val pb = new ProgressBar("populating", 100)
  pb.start()
  pb.maxHint(coords.length)

  val locs = for (
    coord <- coords
  ) yield {
    val ll = new LatLng(coord.lat, coord.lng)
    val results = PlacesApi.nearbySearchQuery(context, ll)
      //.rankby(RankBy.DISTANCE)
      .radius(maxDist)
      .await()
      .results
    if (results.isEmpty) coord else {
      val venues = results.map{ result =>
        new Venue(result.name, result.types, result.geometry.location.lat, result.geometry.location.lng)
      }.sortBy(_.dist(coord))
      coord.copy(venues = venues)
    }
  }

  saveLocations(locs, locFile)
}