package edu.arizona.sista.twitter4food

import java.util.{Calendar, Date, ArrayList, TimeZone}
import java.text.SimpleDateFormat
import collection.JavaConversions._
import edu.arizona.sista.processors.corenlp._
import Mixins._
import scala.util.matching.Regex
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import duration.Duration
import scala.io.Source
import Mixins._

/**
 * Created by dfried on 1/1/14.
 */
case class Tweet(var text: String = null,
                 var userHandle:String = null,
                 var userName: String = null,
                 var userId: Long = -1,
                 var userLocation: String = null,
                 var utcOffset: Int = -1,
                 var timeZone: String = null,
                 var language: String = null,
                 var postTime: Date = null,
                 var offsetPostTime: Date = null,
                 var geoLocation: String = null,
                 var place: String = null,
                 var normalizedLocation: String = null,
                 var tokens: java.util.List[String] = null,
                 var sentiment: Int = 0,
                 var allTokensTopic: Int = -1,
                 var hashtagTokensTopic: Int = -1,
                 var foodTokensTopic: Int = -1,
                 var foodHashtagTokensTopic: Int = -1,
                 var dayOfWeek: Int = -1,
                 val hourOfDay: Int = -1,
                 val monthOfYear: Int = -1) {

  def hashtags = tokens filter { case Tweet.hashtagPattern() => true; case _ => false}

  def formatFloat(x: Float) = "%.4f" format x
}

object Tweet {
  // match any sequence of unicode l
  val tokenPattern: Regex = "[#]?[\\p{L}\\p{M}\\p{N}]+".r
  val punctuationPattern: Regex = "\\.|,|\\?|!|!!|!!!|:|;".r
  val hashtagPattern = "#[\\p{L}\\p{M}\\p{N}]+".r
  val serializePattern = "(.*)\t\t(.*)\t\t(.*)\t\t(.*)\t\t(.*)".r

  val stopWords: Set[String] = Source.fromURL(getClass.getResource("stoplists/twitter_minimal.txt")).getLines.map(_.stripLineEnd).toSet
}

object TweetParser {
  val dateFormat = new SimpleDateFormat("EEE MMM dd HH:mm:ss zzz yyyy")

  val tweetParser = new TweetParser

  def parseTweetFile(source: Source): Seq[Tweet] = {
    val tweets = for {
      lines: Seq[String] <- source.getLines.grouped(3)
    } yield tweetParser.parseTweetLines(lines)
    tweets.toList
  }

  def parseTweetFile(filename: String): Seq[Tweet] = {
    try {
      parseTweetFile(Utils.loadFile(filename))
    } catch {
      case e: Exception => { println(filename); System.err.println(filename); throw e }
    }
  }

  def parallelParseTweetFile(filename: String, nrOfWorkers: Int = 5, maxWait: Duration = Duration.Inf): Seq[Tweet] = {
    // this is so much easier than akka!
    // group the lines into a group for each tweet, converting all iterators to lists
    val groupedLines = Utils.loadFile(filename).getLines.toList.grouped(3).toList
    // split these into bundles based on the number of workers
    val workBundles = groupedLines.grouped(groupedLines.length / nrOfWorkers).toList
    // map out a future to each worker in parallel
    val futures = workBundles.par.map(
      (block) => Future[Seq[Tweet]] { new TweetParser().parseBlockOfTweets(block) }
    )
    // change list of futures into future of a list, wait on it, and then flatten into list of tweets
    Await.result(Utils.all(futures.toList), maxWait).flatten
  }

  def saveTokenizedTweets(tweets: Seq[Tweet], filename: String): Unit = {
    Utils.printToFile(new java.io.File(filename))(p => {
      tweets map (tweet => tweet.tokens.mkString(" ")) foreach (p.println)
    })
  }

  def serializeTweets(tweets: Seq[Tweet], filename: String): Unit = {
    Utils.printToFile(new java.io.File(filename))(p => {
      tweets foreach (p.println)
    })
  }

  def main(args: Array[String]) {
    if (args.length < 2) {
      println("Tweet tokenizer\nusage: inFile outFile")
    } else {
      val tweets = parallelParseTweetFile(args(0))
      serializeTweets(tweets, args(1))
    }
  }
}

class TweetParser(val sentimentClassifier: Option[SentimentClassifier] = None,
                  val topicModels: Map[TokenType, LDA] = Map()) {
  val processor = new CoreNLPProcessor()
  val sentimentProc = new SentimentProcessor

  val geotagger = new GeoTagger()

  val localDateFormat = new SimpleDateFormat("EEE MMM dd HH:mm:ss zzz yyyy")

  def parseBlockOfTweets(blockOfLines: Seq[Seq[String]]): Seq[Tweet] =
    blockOfLines map parseTweetLines

  def parseBlockOfTweets(blockOfLines: Iterator[Seq[String]]): Iterator[Tweet] =
    blockOfLines map parseTweetLines

  def parseTweetsStreaming(source: Source): Stream[Tweet] = {
    source.getLines().grouped(3).toStream.map(parseTweetLines)
  }

  def parseTweetLines(lines: Seq[String]): Tweet = {
    var it = (lines(0).split('\t')).iterator
    val screenName = it.next
    val name = it.next
    val id = it.next.toLong
    val location = it.next
    val followerCount = it.next.toInt
    val utcOffset = it.next.toInt
    val timeZone = it.next
    val createdAt = localDateFormat.parse(it.next)
    val language = it.next
    it = (lines(1).split('\t')).iterator
    val dateString = it.next
    val tweetTime = localDateFormat.parse(dateString)
    val geoLocation = it.next
    val place = it.next
    val text = lines(2).stripLineEnd
    //val locationInfo = LocationInfo(geotagger.normalizeLocation(location, timeZone), location.toStringOpt,
    // geoLocation.toStringOpt, place.toStringOpt)

    val offsetTime: Date = utcOffset match {
      case -1 => null
        // n is seconds, convert to milliseconds and get the offset
      case n => new Date(tweetTime.getTime + (n * 1000))
    }

    val (dayOfWeek, hourOfDay, monthOfYear) = offsetTime match {
      case null => (-1, -1, -1)
      case _ => {
        val c = Calendar.getInstance(TimeZone.getTimeZone("UTC"))
        c.setTime(offsetTime)
        (c.get(Calendar.DAY_OF_WEEK) - 1, c.get(Calendar.HOUR_OF_DAY), c.get(Calendar.MONTH))
      }
    }

    val tokens: Array[String] = processor.mkDocument(text).sentences.map(_.words).flatten.map(_.toLowerCase)

    val tokensAL = new java.util.ArrayList[String](tokens.size)
    for (token <- tokens) tokensAL.add(token)

    val tweet =  new Tweet(text = text,
      userHandle = screenName,
      userName = name,
      userId = id,
      userLocation = location,
      utcOffset = utcOffset,
      timeZone = timeZone,
      //userCreatedAt = createdAt,
      language = language,
      postTime = tweetTime,
      offsetPostTime = offsetTime,
      geoLocation = geoLocation.toStringOpt.map(_.replaceFirst("\\|", ",")).getOrElse(null),
      place = place,
      normalizedLocation = geotagger.normalizeLocation(location, timeZone).getOrElse(null),
      tokens = tokensAL,
      dayOfWeek = dayOfWeek,
      hourOfDay = hourOfDay,
      monthOfYear = monthOfYear)

    sentimentClassifier match {
      case Some(clf) => {
        tweet.sentiment = clf.predict(clf.unlabelledDatum(clf.features(tokens)))
      }
      case _ =>
    }

    topicModels.get(AllTokens) match {
      case Some(model) => tweet.allTokensTopic = model.mostLikelyTopic(tokens filter (AllTokens.okToken _))
      case None =>
    }

    topicModels.get(HashtagTokens) match {
      case Some(model) => tweet.hashtagTokensTopic = model.mostLikelyTopic(tokens filter (HashtagTokens.okToken _))
      case None =>
    }

    topicModels.get(FoodTokens) match {
      case Some(model) => tweet.foodTokensTopic = model.mostLikelyTopic(tokens filter (FoodTokens.okToken _))
      case None =>
    }

    topicModels.get(FoodHashtagTokens) match {
      case Some(model) => tweet.foodHashtagTokensTopic = model.mostLikelyTopic(tokens filter (FoodHashtagTokens.okToken _))
      case None =>
    }

    tweet
  }

}
