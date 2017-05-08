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
  val toKeep = 5 // only keep 5 closest / most relevant Places
  // A set of relevant Google Places types and FourSquare categories to compare against for relevance
  val relevantTypes = io.Source.fromFile(config.getString("classifiers.overweight.location_types"))
    .getLines().map(_.stripLineEnd).toSet

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
    val ll = new LatLng(coord.lat, coord.lng) // LatLng to search near
    val results = PlacesApi.nearbySearchQuery(context, ll)
      .radius(maxDist) // at most maxDist away
      .await() // submit query
      .results // get results of query, if any
    // wait to avoid stepping over API rate limit
    Thread.sleep(sleepTime)
    // Sort results by relevance and add them to coord's venues
    if (results.isEmpty) coord else {
      val venues = results.map{ result =>
        new Venue(result.name, result.types, result.geometry.location.lat, result.geometry.location.lng)
      }.sortBy{v =>
        // near distance is best, but when multiple places in exact same location, pick food-related ones
        (v.dist(coord), v.types.toSet.intersect(relevantTypes).isEmpty)
      }.take(toKeep) // Anything past these is less likely to be relevant
      coord.copy(venues = venues) // Everything is the same but now with venues
    }
  }



  saveLocations(locs, locFile)
}