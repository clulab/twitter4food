package edu.arizona.sista.twitter4food

import scala.io.Source

/**
 * Created by dfried on 2/1/14.
 */
object Datasets {
  object ResourcePaths {
    val kff = "target_data/kff_combined.csv"
    val political = "target_data/proportion_liberal.txt"
    val cdc2010 = "target_data/cdc_obesity_2010.csv"
    val cdc2012 = "target_data/cdc_obesity_2012.csv"
    val illiteracy = "target_data/literacy_2003.csv"

    def source(resourcePath: String): Source =
      Source.fromURL(getClass.getResource(resourcePath))
  }

  val abbvByRegionCoarse =  Map("midwest" -> List("ND", "SD", "NE", "KS", "MN", "IA", "MO", "WI", "IL", "MI", "IN",
    "OH"), // 12
    "northeast" -> List("PA", "NY", "NJ", "NH", "ME", "VT", "MA", "RI", "CT"), // 9
    "south" -> List("OK", "TX", "AR", "LA", "KY", "TN", "MS", "AL", "FL", "GA", "SC", "NC", "VA", "WV", "DC",
      "DE", "MD"), // 18
    "west" -> List("WA", "OR", "CA", "AK", "HI", "MT", "ID", "WY", "NV", "UT", "CO", "AZ", "NM")) // 13

  val abbvByRegionFine = Map("midwest:en_central" -> List("WI", "IL", "MI", "IN", "OH"),
    "midwest:wn_central" -> List("ND", "SD", "NE", "KS", "MN", "IA", "MO"),
    "northeast:middle_atlantic" -> List("PA", "NY", "NJ"),
    "northeast:new_england" -> List("NH", "ME", "VT", "MA", "RI", "CT"),
    "south:es_central" -> List("KY", "TN", "MS", "AL"),
    "south:south_atlantic" -> List("FL", "GA", "SC", "NC", "VA", "WV", "DC", "DE", "MD"),
    "south:ws_central" -> List("OK", "TX", "AR", "LA"),
    "west:mountain" -> List("MT", "ID", "WY", "NV", "UT", "CO", "AZ", "NM"),
    "west:pacific" -> List("WA", "OR", "CA", "AK", "HI"))

  def invertOneToMany[K,V](map: Map[K, Seq[V]]): Map[V, K] = for {
    (k, ls) <- map
    l <- ls
  } yield l -> k

  lazy val regionsCoarse: Map[String, String] = invertOneToMany(abbvByRegionCoarse)
  lazy val regionsFine: Map[String, String] = invertOneToMany(abbvByRegionFine)

  def parseKFF(file: Source): (Map[String, Float], Map[String, Float]) = {
    val lines = file.getLines.drop(1).toList

    def parseLine(line: String) = {
      val tokens = line.stripLineEnd.split('\t')
      (tokens(0), tokens(1).toFloat, tokens(2).toFloat)
    }

    def tplMapper(accessor: ((String, Float, Float)) => (Float)) =
    { tpl: (String, Float, Float) => (tpl._1, accessor(tpl)) }

    val diabetes = (lines map parseLine map tplMapper(_._2)).toMap

    val obesity = (lines map parseLine map tplMapper(_._3)).toMap

    (diabetes, obesity)
  }

  def parseCDC(file: Source): Map[String, Float] = {
    val lines = file.getLines.drop(1).toList

    def parseLine(line: String) = {
      val tokens = line.stripLineEnd.split(',')
      (tokens(0), tokens(1).toFloat)
    }

    (lines map parseLine).toMap
  }

  def parsePolitical(file: Source) = {
    val lines = file.getLines.toList

    def parseLine(line: String) = {
      val tokens = line.stripLineEnd.split("\\s+")
      (tokens(0), tokens(1).toFloat)
    }

    (lines map parseLine).toMap
  }

  def parseIlliteracy(file: Source) = parseCDC(file)

  lazy val (diabetes, overweight) = parseKFF(ResourcePaths.source(ResourcePaths.kff))
  lazy val political: Map[String, Float] = parsePolitical(ResourcePaths.source(ResourcePaths.political))
  lazy val cdc2010: Map[String, Float] = parseCDC(ResourcePaths.source(ResourcePaths.cdc2010))
  lazy val cdc2012: Map[String, Float] = parseCDC(ResourcePaths.source(ResourcePaths.cdc2012))
  lazy val illiteracy: Map[String, Float] = parseCDC(ResourcePaths.source(ResourcePaths.illiteracy))
}
