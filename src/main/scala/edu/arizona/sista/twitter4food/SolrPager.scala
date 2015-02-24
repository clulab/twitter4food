package edu.arizona.sista.twitter4food

import jp.sf.amateras.solr.scala._
import java.util.Date
import java.util.ArrayList
import collection.JavaConversions._
import jp.sf.amateras.solr.scala.Order.asc

/**
 * Created by dfried on 4/28/14.
 */
class SolrPager(val connectionURL: String = "http://localhost:8983/solr/twitter4food") {
  val client = new SolrClient(connectionURL)

  def pagedRequest(query: QueryBuilder, pageSize: Int = 10000, maxRows: Option[Int] = None): Stream[Tweet] = {
    def request(offset: Int) = query.rows(pageSize).start(offset * pageSize).getResultAs[Tweet]()
    val N = if (maxRows.isDefined && maxRows.get < request(0).numFound.toInt)
      maxRows.get
    else
      request(0).numFound.toInt
    (for {
      offset <- (0 to (N / pageSize)).toStream
      doc <- request(offset).documents
    } yield doc).take(N)
  }

  private def makeCityQueryPair(cityName: String) = {
    def spRep = cityName.replace(" ", "\\ ")
    cityName -> s"place:*$spRep* OR userLocation:*$spRep*"
  }

  val rankedCityQueries = List(
    "New York City" -> "place:*New\\ York* OR userLocation:*NYC* OR userLocation:*New\\ York\\ City*",
    makeCityQueryPair("Los Angeles"),
    makeCityQueryPair("Chicago"),
    makeCityQueryPair("Houston"),
    makeCityQueryPair("Philadelphia"),
    makeCityQueryPair("Phoenix"),
    makeCityQueryPair("San Antonio"),
    makeCityQueryPair("San Diego"),
    makeCityQueryPair("Dallas"),
    makeCityQueryPair("San Jose"),
    makeCityQueryPair("Austin"),
    makeCityQueryPair("Indianapolis"),
    makeCityQueryPair("Jacksonville"),
    makeCityQueryPair("San Francisco"),
    makeCityQueryPair("Columbus")
  )
  def queryTokens(query: QueryBuilder): Stream[Seq[String]] = pagedRequest(query).map(_.tokens.toList)

  def queryTokens(queryString: String): Stream[Seq[String]] = queryTokens(client.query(queryString))

  // return all tweets in the database
  def tweets = pagedRequest(client.query("*:*"))

  // return those tweets with a normalized location
  def tweetsWithLocation = pagedRequest(client.query("normalizedLocation: [* TO *]"))

  def tweetsGroupedByLocation = tweetsWithLocation.groupBy(tweet => tweet.normalizedLocation)

  def tweetsGroupedByCity(numTopCities: Int) =
    rankedCityQueries.take(numTopCities).toMap.mapValues(queryString => pagedRequest(client.query(queryString)))

  def tweetsGroupedByRegion(regionLookup: Map[String, String] = Datasets.regionsCoarse) =
    tweetsWithLocation.groupBy(tweet => regionLookup(tweet.normalizedLocation))

}

object SolrPager {
  def randomSort(query: QueryBuilder, seed: Option[Int] = None) = {
    val fieldName = "random_"+seed.getOrElse(util.Random.nextInt(10000))
    query.sortBy(fieldName, asc)
  }
}
