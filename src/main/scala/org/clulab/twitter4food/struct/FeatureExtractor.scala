package org.clulab.twitter4food.struct

import java.io.{BufferedReader, FileReader}

import org.clulab.learning.{Datum, LiblinearClassifier, RVFDatum}
import org.clulab.struct.{Counter, Counters, Lexicon}
import org.clulab.twitter4food.util.{FileUtils, Tokenizer}
import org.clulab.twitter4food.struct.Normalization._
import cmu.arktweetnlp.Tagger._
import com.typesafe.config.ConfigFactory
import org.clulab.twitter4food.featureclassifier.HumanClassifier
import org.clulab.twitter4food.lda.LDA
import org.slf4j.LoggerFactory

/**
  * Created by Terron on 2/9/16.
  *
  * Designed to be used in tandem with a classifier, with the features
  * to be included "turned on" with the inputted parameters. The features
  * listed here are fairly general - custom features can be added with
  * the polymorphic mkDatum function.
  *
  * All parameters are flags for which features should be used.
  */
class FeatureExtractor (
  val useUnigrams: Boolean = false,
  val useBigrams: Boolean = false,
  val useTopics: Boolean = false,
  val useDictionaries: Boolean = false,
  val useEmbeddings: Boolean = false,
  val useCosineSim: Boolean = false,
  val useFollowers: Boolean = false,
  val datumScaling: Boolean = false) {

  import FeatureExtractor._

  val config = ConfigFactory.load()
  val logger = LoggerFactory.getLogger(this.getClass)
  logger.info(s"useUnigrams=$useUnigrams, " +
    s"useBigrams=$useBigrams, " +
    s"useTopics=$useTopics, " +
    s"useDictionaries=$useDictionaries, " +
    s"useEmbeddings=$useEmbeddings, " +
    s"useCosineSim=$useCosineSim, " +
    s"useFollowers=$useFollowers, " +
    s"datumScaling=$datumScaling"
  )

  // LDA topic model
  var topicModel: Option[LDA] = if (useTopics) {
    Some(LDA.load(config.getString("lda.topicModel")))
  } else None

  // Dictionaries : Map[Label -> Map[LexiconName -> Lexicon]]
  // Messy, but useful data structure.
  var lexicons: Option[Map[String, Map[String, Lexicon[String]]]] = None

  // Embeddings
  var idfTable: Option[Counter[String]] = None
  var overweightVec: Option[Counter[String]] = None

  // Followers
  val relationsFileStr = config.getString("classifiers.features.followerRelations")
  var handleToRelations = Map[String, Seq[String]]()
  val relationsFile = scala.io.Source.fromFile(relationsFileStr)
  for (line <- relationsFile.getLines) {
    val handles = line.split("\t")
    handleToRelations += (handles(0) -> handles.slice(1, handles.length))
  }
  relationsFile.close
  val hon = try {
    Some(LiblinearClassifier.loadFrom[String, String](config.getString("classifiers.overweight.humanOrNot")))
  } catch {
    case e: Exception =>
      logger.debug(s"Human classifier not found at ${config.getString("classifiers.overweight.humanOrNot")}!")
      None
  }

  val accountsFileStr = config.getString("classifiers.features.followerAccounts")
  val followerAccounts = if (useFollowers) FileUtils.load(accountsFileStr) else Map[TwitterAccount, String]()

  var handleToFollower = Map[String, TwitterAccount]()
  for ((account, _) <- followerAccounts)
    handleToFollower += (account.handle -> account)

  /**
    * Copy a [[Counter]] so it's not accidentally overwritten
    */
  def copyCounter[T](counter: Counter[T]): Counter[T] = counter.map(kv => kv._2)

  /** Reads a sequence of filenames for each label as a sequence of lexicons
    *
    * @param lexiconMap A map of (label -> sequence of filenames)
    * @return Unit
    */
  def setLexicons(lexiconMap: Map[String, Seq[String]]) = {
    val l = lexiconMap map {
      case (k, v) => (k, v.map(fileName => {
        val lexName = fileName.substring(fileName.lastIndexOf("/") + 1,
          fileName.indexOf("."))
        (lexName, Lexicon.loadFrom[String](fileName))
      }).toMap)
    }
    this.lexicons = Some(l)
  }

  /**
    * Additional method call for adding additional features
    * outside of what's presented here.
    */
  def mkDatum(account: TwitterAccount, label: String,
    counter: Counter[String]): Datum[String, String] = {
    new RVFDatum[String, String](label, mkFeatures(account) + counter)
  }

  /**
    * Ultimately what should be called by the classifier when training.
    *
    * @param account TwitterAccount to extract features from
    * @param label Classification label associated with this account
    */
  def mkDatum(account: TwitterAccount, label: String): Datum[String, String] = {
    new RVFDatum[String, String](label, mkFeatures(account))
  }

  /**
    * Generates the feature counter for this account
    *
    * @param account
    * @return Counter of all features signified by constructor flags
    */
  def mkFeatures(account: TwitterAccount): Counter[String] = {
    val counter = new Counter[String]

    val tweets = for (t <- account.tweets) yield t.text.trim.split(" +")
    val description = account.description.trim.split(" +")

    var unigrams: Option[Counter[String]] = None

    if (useUnigrams | useDictionaries | useCosineSim)
      unigrams = Some(ngrams(1, tweets.map(filterStopWords), description))
    if (useUnigrams) {
      counter += unigrams.get
    }
    if (useBigrams)
      counter += ngrams(2, tweets, description)
    if (useTopics)
      counter += topics(tweets)
    if (useDictionaries)
      counter += dictionaries(tweets, description, account, unigrams)
    if (useEmbeddings){

    } // TODO: how to add embeddings as a feature if not returning a counter?
    if (useCosineSim)
      counter += cosineSim(unigrams, tweets, description)

    // must scaleByDatum now to keep scaling distinct from follower features
    if (datumScaling)
      scaleByDatum(counter, 0.0, 1.0)

    if (useFollowers) {
      // make deep copy of counter, range 0-1
      val mc = new Counter[String]
      counter.toSeq.foreach(kv => mc.setCount(kv._1, kv._2))

      val fc = followers(account)

      // if scaling by datum, followers will have range 0-1 like main; otherwise, scale followers to have same total
      // feature count as the main features
      if (datumScaling) scaleByDatum(fc, 0.0, 1.0) // followers range 0-1
      else scaleByCounter(fc, mc)

      counter += fc

      // combined scores should range 0-1 if datumScaling
      if (datumScaling) scaleByDatum(counter, 0.0, 1.0)

      counter += appendPrefix("follower-", fc) + appendPrefix("main-", mc)
    }

    // remove zero values for sparse rep
    counter.filterValues(v => v != 0.0)
  }

  def mkFeaturesFollowers(account: TwitterAccount): Counter[String] = {
    var counter = new Counter[String]
    val tweets = for (t <- account.tweets) yield t.text.trim.split(" +")
    val description = account.description.trim.split(" +")

    var unigrams: Option[Counter[String]] = None

    if (useUnigrams | useDictionaries | useCosineSim)
      unigrams = Some(ngrams(1, tweets.map(filterStopWords), description))
    if (useUnigrams)
      counter += unigrams.get
    if (useBigrams)
      counter += ngrams(2, tweets, description)
    if (useTopics)
      counter += topics(tweets)
    if (useDictionaries)
      counter += dictionaries(tweets, description, account, unigrams)
    if (useEmbeddings){

    } // TODO: how to add embeddings as a feature if not returning a counter?
    if (useCosineSim)
      counter += cosineSim(unigrams, tweets, description)
    // Calling mkFeatures on followers will end up being endlessly recursive

    counter
  }

  // Helper function for mapping a prefix onto all labels in a counter (to add the "follower_" prefix)
  def appendPrefix (prefix: String, counter: Counter[String]): Counter[String] = {
    val temp = new Counter[String]()
    for ((label, score) <- counter.toSeq)
      temp.setCount(prefix + label, score)
    temp
  }

  def setCounts(words: Seq[String], counter: Counter[String]) = {
    words.foreach(word => counter.incrementCount(word, 1))
  }

  def tokenSet(tt: Array[TaggedToken]) = tt.map(t => t.token)

  /**
    * Adds ngrams from account's description and tweets with raw frequencies as weights.
    *
    * @param n Degree of n-gram (e.g. 1 refers to unigrams)
    * @param description Text description of [[TwitterAccount]]
    * @return counter
    */
  def ngrams(n: Int, tweets: Seq[Array[String]], description: Array[String]): Counter[String] = {
    val counter = new Counter[String]

    // Extract ngrams
    def populateNGrams(n: Int, text: Array[String]): Seq[String] = {
      text.sliding(n).toList.map(ngram => ngram.mkString(" "))
    }

    // Filter ngrams by their POS tags
    setCounts(populateNGrams(n, description), counter)

    // Filter further by ensuring we get English tweets and non-empty strings
    tweets.foreach{ tweet =>
      setCounts(populateNGrams(n, tweet), counter)
    }

    counter
  }

  /**
    * Adds the flagged features to a new counter for domain adaptation.
    *
    * @param account [[TwitterAccount]] whose followers will be annotated
    * @return counter
    */
  def followers(account: TwitterAccount): Counter[String] = {
    // Find this account's active followers
    val followerHandles = if (handleToRelations.contains(account.handle)) handleToRelations(account.handle) else List[String]()

    // Find the TwitterAccount object corresponding to these handles
    val followers = followerHandles.flatMap(f =>
      if (handleToFollower.contains(f)) Some(handleToFollower(f)) else None
    )

    val filtered = if (hon.nonEmpty) {
      val hc = new HumanClassifier() // assume we're using unigrams only
      hc.subClassifier = hon
      val humanPredicted = (followers, hc._test(followers)).zipped
      humanPredicted.filter{ case (follower, predicted) => predicted == "human"}._1
    } else followers

    // Aggregate the counter for the followers using the other features being used
    val followerCounters = for (follower <- filtered.par) yield mkFeaturesFollowers(follower)

    val followerCounter = new Counter[String]
    followerCounters.seq.foreach(fc => followerCounter += fc)

    followerCounter
  }

  /**
    * Add a feature for each topic in the topic model, and count instances in the account's tweets
    *
    * @param tweets Tokenized, filtered [[Tweet]] text
    * @return a [[Counter]] of topics for this account
    */
  def topics(tweets: Seq[Array[String]]): Counter[String] = {
    // no topic model available
    if (topicModel.isEmpty) return null

    val tm = topicModel.get
    val topics = new Counter[String]
    tweets.foreach(tweet => topics.incrementCount("topic_" + tm.mostLikelyTopic(tweet)))

    topics
  }

  /**
    * Functions like unigrams but constrained to words in dictionaries.
    * NOTE: Adding classifier wise dictionaries since the number of classifiers
    *       is very manageable. -@adikou
    *
    * @return counter - Return one counter fine-tuned for a particular classifier
    */
  def dictionaries(tweets: Seq[Array[String]],
    description: Array[String],
    account: TwitterAccount,
    ngramCounter: Option[Counter[String]]): Counter[String] = {

    val result = new Counter[String]()
    if(lexicons.isEmpty) return result

    // Classifier type
    val cType = lexicons.get.keys.head match {
      case "M" | "F" => "gender"
      case "Overweight" | "Not overweight" => "overweight"
      case "human" | "org" => "human"
      case "asian" | "hispanic" | "white" | "black" => "race"
    }

    if((cType equals "human") || (cType equals "gender")) {
      lexicons.get foreach {
        case (k, v) =>
          v foreach {
            case (lexName, lexicon) =>
              var nS = 0

              account.name.toLowerCase.split("\\s+").zipWithIndex.foreach {
                case (n, i) =>
                  // If first name
                  println(n)
                  if (i == 0 & lexicon.contains(n)) nS += 1
                  else if (lexName.contains("last") & lexicon.contains(n)) nS += 1
              }

              // Check substrings for handles
              val matches = lexicon.keySet.filter(account.handle.toLowerCase.drop(1).contains(_))
              nS += matches.size

              val dS = if (!lexName.contains("name")) description.count(lexicon.contains) else 0

              if(dS + nS > 0) result.incrementCount(s"lex_${k}_$lexName", dS + nS)

          }
      }
    }
    else if(cType equals "race") {

    }
    else if(cType equals "overweight") {
      // Load dictionaries
      val foodWordsFile = scala.io.Source
        .fromFile(config.getString("classifiers.features.foodWords"))
      val foodWords = foodWordsFile.getLines.toSet
      foodWordsFile.close

      val hashtagsFile = scala.io.Source
        .fromFile(config.getString("classifiers.features.hashtags"))
      val hashtags = hashtagsFile.getLines.toSet
      hashtagsFile.close

      // Filter ngrams
      var temp = if (ngramCounter.nonEmpty) copyCounter(ngramCounter.get) else ngrams(1, tweets, description)
      temp = temp.filter( tup => foodWords.contains(tup._1) || hashtags.contains(tup._1))

      // Copy into counter with prefix to indicate this is a different feature
      temp.keySet.foreach( word => result.setCount("dict_" + word, temp.getCount(word)))
    }
    result
  }

  def embeddings(account: TwitterAccount): Map[TwitterAccount, Array[Float]] = {
    null
  }

  // Loads the idf table for the database of random tweets
  private def loadTFIDF(): Unit = {
    // Initialize
    val randomCounter = new Counter[String]()
    var i = 0
    var N = 0
    var randomFile = new BufferedReader(new FileReader(config.getString("classifiers.features.random_tweets")))
    var numLines = 0
    while (randomFile.readLine() != null) numLines += 1
    randomFile = new BufferedReader(new FileReader(config.getString("classifiers.features.random_tweets")))

    // Start progress bar
    val pb = new me.tongfei.progressbar.ProgressBar("loadTFIDF()", 100)
    pb.start()
    pb.maxHint(numLines/3)
    pb.setExtraMessage("Loading idf table of tweets...")

    // Iterate over file
    var line = ""
    while ( { line = randomFile.readLine ; line != null } ) {
      // Actual tweet text is every third line
      if (i % 3 == 2) {
        // Filter words based on FeatureExtractor for consistency
        val filteredTokens = filterTags(Tokenizer.annotate(line.toLowerCase))
        filteredTokens.distinct.foreach(token => randomCounter.incrementCount(token))
        N += 1
        pb.step()
      }
      i += 1
      i %= 3
    }

    randomFile.close()
    pb.stop()

    randomCounter.keySet.foreach(word => randomCounter.setCount(word, scala.math.log(N / randomCounter.getCount(word))))

    idfTable = Some(randomCounter)

    // Do the same for overweight corpus, using idf table for idf value
    val overweightCounter = new Counter[String]()
    var overweightFile = new BufferedReader(new FileReader(config.getString("classifiers.features.overweight_corpus")))
    numLines = 0
    while (overweightFile.readLine() != null) numLines += 1
    overweightFile.close()

    overweightFile = new BufferedReader(new FileReader(config.getString("classifiers.features.overweight_corpus")))

    val pb2 = new me.tongfei.progressbar.ProgressBar("loadTFIDF()", 100)
    pb2.start()
    pb2.maxHint(numLines/3)
    pb2.setExtraMessage("Loading tfidf vector for overweight corpus...")

    i = 0
    line = ""
    while ( { line = overweightFile.readLine ; line != null } ) {
      if (i % 3 == 2) {
        // No need to keep track of what's been seen since we're accumulating tf here
        val filteredTokens = filterTags(Tokenizer.annotate(line.toLowerCase))
        filteredTokens.foreach(t => overweightCounter.incrementCount(t))
        pb2.step()
      }
      i += 1
      i %= 3
    }

    overweightFile.close()
    pb.stop()

    overweightCounter.keySet.foreach(word =>
      overweightCounter.setCount(word, math.log(overweightCounter.getCount(word)) * (1 + idfTable.get.getCount(word)))
    )
    overweightVec = Some(overweightCounter)
  }

  /**
    * A single feature (that is, a counter with the singular entry ("cosineSim" -> cosineSim).
    * Calculates the cosine similarity between the TFIDF vector of the account's description
    * and tweets and the TFIDF vector of the overweight corpus.
    *
    * @return counter
    */
  def cosineSim(ngramCounter: Option[Counter[String]], tweets: Seq[Array[String]],
    description: Array[String]): Counter[String] = {

    if (idfTable.isEmpty || overweightVec.isEmpty) loadTFIDF()

    val accountVec = if (ngramCounter.nonEmpty) copyCounter(ngramCounter.get) else ngrams(1, tweets, description)

    accountVec.keySet.foreach(word =>
      accountVec.setCount(word, math.log(accountVec.getCount(word)) * (1 + idfTable.get.getCount(word)))
    )

    // Calculate cosine similarity
    val result = new Counter[String]()
    result.setCount("cosineSim", Counters.cosine(accountVec, overweightVec.get))

    result
  }
}

object FeatureExtractor {
  val config = ConfigFactory.load()
  val logger = LoggerFactory.getLogger(this.getClass)
  val stopWordsFile = scala.io.Source.fromFile(config.getString("classifiers.features.stopWords"))
  val stopWords = stopWordsFile.getLines.toSet
  stopWordsFile.close

  // NOTE: all features that run over description and tweets should probably apply this for consistency
  def filterTags(tagTok: Array[TaggedToken]): Array[String] = {
    // val stopWordsFile = scala.io.Source.fromFile(config.getString("classifiers.features.stopWords"))
    // val stopWords = stopWordsFile.getLines.toSet
    // stopWordsFile.close
    // tagTok.filter(tt => !"@UGD,~$".contains(tt.tag)
    // && "#NVAT".contains(tt.tag) && !stopWords.contains(tt.token))
    val emptyString = "^[\\s]*$"
    val lumped = for (tt <- tagTok) yield {
      (tt.token, tt.tag) match {
        case (site, "U") => Some("<URL>")
        case (handle, "@") => Some("<@MENTION>")
        case (number, "$") => Some("<NUMBER>")
        case (garbage, "G") => None
        case (rt, "~") => None
        case (token, tag) if token.matches(emptyString) => Some(token)
        case (token, tag) => None
      }
    }
    lumped.flatten
  }

  def filterStopWords(tokens: Array[String]): Array[String] = tokens filterNot stopWords.contains


}