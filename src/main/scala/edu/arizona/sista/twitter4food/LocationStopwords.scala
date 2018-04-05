package edu.arizona.sista.twitter4food

import io.Source

/**
 * Created by dfried on 5/2/14.
 */
object LocationStopwords {
  private lazy val cities: Set[String] = {
    println("calculating")
    val cityFile = Source.fromURL(getClass.getResource("top_cities.txt"))
    cityFile.getLines().map(_.toLowerCase().replaceAll("\\s+|\\.", "")).toSet
  }

  private lazy val states: Set[String] = {
    val stateFile = Source.fromURL(GeoTagger.stateResource)
    stateFile.getLines().flatMap(_.toLowerCase().split("\t").map(_.replaceAll("\\s+|\\\\|\\.", ""))).toSet
  }

  private val others = Set("nyc", "vegas", "dc", "sanfran", "carlolina", "hampshire", "dakota", "los", "las", "san",
    "jersey", "philly", "york", "phx", "atl", "indy", "santafe", "stl", "kc", "princeton", "atx", "okc", "westport",
    "miamibeach", "southflorida", "brickell", "oshkosh", "rva", "oxford", "charlottefood", "dtla", "sf", "santamonica",
    "sxsw", "washingtondc", "harlem", "manhattan", "toronto", "dtphx", "wynwood", "waikiki", "hawaiian",
    "belfastmaine", "montclair", "caldwellnj", "hoboken", "bronx", "brooklyn",  "jacksonhole", "columbusohio",
    "easton", "columbusga", "uptowndallas", "ilovejax", "jax", "phillyfood", "littleitalysd", "diego", "lajolla",
    "francisco", "mission", "sfo", "sj")

  private lazy val withoutHashtags = cities ++ states ++ others

  lazy val stopWords: Set[String] = {
    withoutHashtags ++ withoutHashtags.map(word => "#" + word)
  }

}

