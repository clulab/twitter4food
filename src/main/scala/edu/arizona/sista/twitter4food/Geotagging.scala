package edu.arizona.sista.twitter4food

import Geotagging._
import com.fasterxml.jackson.databind.node.{ObjectNode,ArrayNode}
import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import scala.collection.JavaConversions._

case class Location(lat:Double, lng:Double)

/**
 * Created by dfried on 4/4/14.
 * direct port of https://github.com/pegler/pytzwhere
 */
object Geotagging {
  type Polygon = List[Location]

  val STEPSIZE = 1

  def locationInsidePolygon(location: Location, poly: Polygon) = {
    val n = poly.size
    val Location(y, x) = location
    var inside = false
    var (p1x: Double, p1y: Double) = (poly(0).lng, poly(0).lat)
    var xinters = 0d
    for (i <- 1 to n) {
      var (p2x: Double, p2y: Double) = (poly(i % n).lng, poly(i % n).lat)
      if (y > math.min(p1y, p2y))
        if (y <= math.max(p1y, p2y))
          if (x <= math.max(p1x, p2x)) {
            if (p1y != p2y)
              xinters = (y - p1y) * (p2x-p1x) / (p2y - p1y) + p1x
            if (p1x == p2x || x <= xinters)
              inside = !inside
          }
      p1x = p2x
      p1y = p2y
    }
    inside
  }

  def binValue(value: Double): Double = math.floor(value / STEPSIZE).toInt * STEPSIZE

  def mkShortcuts(accessor: Location => Double)(timezoneToPolys: Map[String, IndexedSeq[Polygon]]): Map[Double, Map[String, Seq[Int]]] = {
    val tuples: Seq[(Double, String, Int)] = for {
      (tzname, polys) <- timezoneToPolys.toSeq
      (poly: Seq[Location], polyIndex: Int) <- polys.zipWithIndex
      vals: Seq[Double] = poly.map(accessor)
      degree <- binValue(vals.min) to binValue(vals.max) by STEPSIZE
    } yield (degree, tzname, polyIndex)
    // make hierarchical index, granular lat or long -> time zone name -> list of indices
    tuples.groupBy(_._1).mapValues(_.groupBy(_._2).mapValues(_.map(_._3)))
  }

  // cast to a JSON map
  def M(o: Any) = o.asInstanceOf[Map[String, Any]]

  def parsePair(pair: Seq[Double]) = Location(pair(1), pair(0))

  def parsePolygons(polygons: List[List[Double]]): List[Polygon] = {
    polygons.map(polygon => polygon.grouped(2).map(parsePair).toList)
  }

  def parseJson(map: ObjectNode) = {
    val featureList: Iterable[JsonNode] = map.get("features").elements().toIterable
    val pairs: Iterable[(String, List[Polygon])] = for {
      feature <- featureList
      tzname = feature.get("properties").get("TZID").asText()
      coordinates = feature.get("geometry").get("coordinates").elements().toList.map(_.elements().toList.map(_.asDouble))
      polygons: List[Polygon] = parsePolygons(coordinates)
    } yield (tzname, polygons)
    pairs.groupBy({case (tzname, _) => tzname}).mapValues(_.map(_._2).flatten.toIndexedSeq)
  }

}

class Geotagging(var jsonPath: String) {

  println("parsing json file")

  val source = Utils.loadFile(jsonPath)

  val mapper = new ObjectMapper
  val jsonAst:ObjectNode = mapper.readValue(source.getLines.mkString("\n"), classOf[ObjectNode])

  val timezoneToPolygons: Map[String, IndexedSeq[Polygon]] = parseJson(jsonAst)

  val lngShortcuts = mkShortcuts(_.lng)(timezoneToPolygons)
  val latShortcuts = mkShortcuts(_.lat)(timezoneToPolygons)

  def tzNameAt(location: Location): Option[String] = {
    val latTZOptions = latShortcuts(binValue(location.lat))
    val latSet = latTZOptions.keySet
    val lngTZOptions = latShortcuts(binValue(location.lat))
    val lngSet = lngTZOptions.keySet
    val possibleTimezones = latSet.intersect(lngSet)
    for (tzname <- possibleTimezones) {
      val polyIndices = latTZOptions(tzname).toSet.intersect(lngTZOptions(tzname).toSet)
      for (polyIndex <- polyIndices) {
        val poly = timezoneToPolygons(tzname)(polyIndex)
        if (locationInsidePolygon(location, poly))
          return Some(tzname)
      }
    }
    return None
  }
}
