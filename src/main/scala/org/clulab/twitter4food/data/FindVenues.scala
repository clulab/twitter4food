package org.clulab.twitter4food.data

import java.io.{BufferedWriter, FileWriter}
import java.net.SocketTimeoutException
import java.util.concurrent.TimeUnit

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

  if(args contains "--help") {
    println("Takes in Tweet locations and uses\nthe Google Places to find nearby venues.\n" +
      "Use '--new' to throw out already-found venues\n(default is to append to existing ones).")
  }
  val append = !(args contains "--new")

  val logger: Logger = LoggerFactory.getLogger(this.getClass)
  if (append) logger.info("Appending to existing venues.") else logger.info("Throwing out any existing venues.")

  val config: Config = ConfigFactory.load
  val sleepTime = 586 // 150000 requests / 24 hours => 576 ms per request + 10 ms for a buffer
  val maxDist = 50 // meters between venues and tweet location
  val toKeep = 5 // only keep 5 closest / most relevant Places
  // A set of relevant Google Places types and FourSquare categories to compare against for relevance
  val relevantTypes = io.Source.fromFile(config.getString("classifiers.overweight.location_types"))
    .getLines().map(_.stripLineEnd).toSet

  // Place to put annotated Locations
  val locFile = config.getString("classifiers.overweight.tweetLocs")
  // Already-annotated locations
  val existing = if (append) loadLocations(locFile).toSet else Set[Location]()
  // Writer to write as we go so we can pick up upon failure.
  val writer = new BufferedWriter(new FileWriter(locFile, append))
  // Locations before Venue annotation (minus existing locations w/ venues)
  val coordsFile = config.getString("classifiers.overweight.tweetCoords")
  val coords = loadLocations(coordsFile).filterNot{c => existing.exists(l => l.id == c.id)}

  // Google Places API key
  val apiKey = scala.io.Source.fromFile(config.getString("places_key")).getLines().next().stripLineEnd
  val context = new GeoApiContext()
    .setApiKey(apiKey) // must have API key for request
    .setReadTimeout(5, TimeUnit.MINUTES) // long time-out
    .setQueryRateLimit(0) // We manually control this to get closer to ideal rate

  val pb = new ProgressBar("populating", 100)
  pb.start()
  pb.maxHint(coords.length)

  val locs = for (
    coord <- coords
  ) yield {
    pb.step()
    val ll = new LatLng(coord.lat, coord.lng) // LatLng to search near
    try {
      val results = PlacesApi.nearbySearchQuery(context, ll)
        .radius(maxDist) // at most maxDist away
        .await() // submit query
        .results // get results of query, if any
      // wait to avoid stepping over API rate limit
      Thread.sleep(sleepTime)
      // Sort results by relevance and add them to coord's venues
      val venued = if (results.isEmpty) coord else {
        val venues = results.map { result =>
          new Venue(result.name, result.types, result.geometry.location.lat, result.geometry.location.lng)
        }.sortBy { v =>
          // near distance is best, but when multiple places in exact same location, pick food-related ones
          (v.dist(coord), v.types.toSet.intersect(relevantTypes).isEmpty)
        }.take(toKeep) // Anything past these is less likely to be relevant
        coord.copy(venues = venues) // Everything is the same but now with venues
      }
      writer.write(s"${venued.toString}\n") // write as we go, in case of failure
    } catch {
      case s: SocketTimeoutException =>
        writer.close()
        pb.stop()
        println("Socket timeout")
        System.exit(0)
      case e: Exception =>
        writer.close()
        pb.stop()
        println(e.toString)
        System.exit(0)
    }
  }

  pb.stop()
}