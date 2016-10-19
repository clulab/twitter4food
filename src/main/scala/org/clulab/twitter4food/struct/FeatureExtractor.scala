package org.clulab.twitter4food.struct

import java.io.{BufferedReader, FileReader}
import java.nio.file.{Files, Paths}

import org.clulab.learning.{Datum, L1LinearSVMClassifier, LiblinearClassifier, RVFDatum}
import org.clulab.struct.{Counter, Counters, Lexicon}
import org.clulab.twitter4food.struct.Normalization._
import org.clulab.twitter4food.util.Utils._
import cmu.arktweetnlp.Tagger._
import com.typesafe.config.ConfigFactory
import org.clulab.twitter4food.featureclassifier.{ClassifierImpl, GenderClassifier, HumanClassifier}
import org.clulab.twitter4food.lda.LDA
import org.clulab.twitter4food.util.FileUtils
import org.slf4j.LoggerFactory

import scala.collection.mutable.ArrayBuffer

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
  val useAvgEmbeddings: Boolean = false,
  val useMinEmbeddings: Boolean = false,
  val useMaxEmbeddings: Boolean = false,
  val useCosineSim: Boolean = false,
  val useFollowers: Boolean = false,
  val useFollowees: Boolean = false,
  val useGender: Boolean = false,
  val useRace: Boolean = false,
  val useHuman: Boolean = false,
  val datumScaling: Boolean = false,
  val customFeatures: (TwitterAccount) => Counter[String] = account => new Counter[String]()) {

  import FeatureExtractor._

  val config = ConfigFactory.load()
  val logger = LoggerFactory.getLogger(this.getClass)
  logger.info(s"useUnigrams=$useUnigrams, " +
    s"useBigrams=$useBigrams, " +
    s"useTopics=$useTopics, " +
    s"useDictionaries=$useDictionaries, " +
    s"useAvgEmbeddings=$useAvgEmbeddings, " +
    s"useMinEmbeddings=$useMinEmbeddings, " +
    s"useMaxEmbeddings=$useMaxEmbeddings, " +
    s"useCosineSim=$useCosineSim, " +
    s"useFollowers=$useFollowers, " +
    s"useFollowees=$useFollowees, " +
    s"useGender=$useGender, " +
    s"useRace=$useRace, " +
    s"useHuman=$useHuman, " +
    s"datumScaling=$datumScaling"
  )

  // LDA topic model
  var topicModel: Option[LDA] = if (useTopics) {
    logger.info("Loading LDA topic model...")
    Some(LDA.load(config.getString("lda.topicModel")))
  } else None

  // Dictionaries : Map[Label -> Map[LexiconName -> Lexicon]]
  // Messy, but useful data structure.
  var lexicons: Option[Map[String, Map[String, Lexicon[String]]]] = None

  // Additional annotations for food words: average calories of foods with this ingredient in the name; average health
  // rating of these foods. These are used in the dictionaries() features
  val (calories, healthiness): (Option[Map[String,Double]], Option[Map[String,Int]]) = if (useDictionaries) {
    val foodAnnotations = scala.io.Source.fromFile(config.getString("classifiers.overweight.annotatedFoodFile"))
    val cMap = scala.collection.mutable.Map[String, Double]()
    val hMap = scala.collection.mutable.Map[String, Int]()
    for {
      line <- foodAnnotations.getLines
      splits = line.split("\t")
    } {
      if (splits(1) != "NULL") cMap(splits(0)) = splits(1).toDouble
      if (splits(2) != "NULL") cMap(splits(0)) = splits(2).toInt
    }
    (Some(cMap.toMap), Some(hMap.toMap))
  } else (None, None)

  // Word2vec word embedding vectors
  val vectors = if (useAvgEmbeddings || useMinEmbeddings || useMaxEmbeddings) loadVectors else None
  // tdidf vector for overweight corpus
  val (idfTable, overweightVec) = if (useCosineSim) loadTFIDF else (None, None)

  // Followees (to be set by setFollowees)
  var handleToFollowees: Option[Map[String, Seq[String]]] = None

  // human classifier for follower filtering
  val humanClassifier = if (useHuman && useFollowers) {
    val modelFile = config.getString("classifiers.overweight.humanClassifier")
    val model = if (Files.exists(Paths.get(modelFile))) {
      val sub = LiblinearClassifier.loadFrom[String, String](modelFile)
      val h = new HumanClassifier(useDictionaries=true, useMaxEmbeddings=true, customFeatures = HumanClassifier.customFeatures)
      h.subClassifier = Option(sub)
      h
    } else {
      // train a fresh classifier
      logger.debug(s"$modelFile not found; attempting to train...")

      val trainingData = FileUtils.load(config.getString("classifiers.human.trainingData")) ++
        FileUtils.load(config.getString("classifiers.human.devData")) ++
        FileUtils.load(config.getString("classifiers.human.testData"))
      // val tmp = new HumanClassifier(useDictionaries=true, useFollowers=true, useMaxEmbeddings=true)
      val tmp = new HumanClassifier(useDictionaries=true, useMaxEmbeddings=true, customFeatures = HumanClassifier.customFeatures)

      // bad to have to load followers possibly multiple times, but this should happen only rarely
      // TODO: different follower files by classifier
      val followers = if (tmp.useFollowers) {
        Option(ClassifierImpl.loadFollowers(trainingData.keys.toSeq))
      } else None
      val followees = if (tmp.useFollowees) {
        Option(ClassifierImpl.loadFollowees(trainingData.keys.toSeq, "human"))
      } else None
      tmp.setClassifier(new L1LinearSVMClassifier[String, String]())
      tmp.train(trainingData.keys.toSeq, trainingData.values.toSeq, followers, followees)
      tmp.subClassifier.get.saveTo(modelFile)
      tmp
    }
    Option(model)
  } else None

  // gender classifier for domain adaptation
  val genderClassifier = if(useGender) {
    val modelFile = config.getString("classifiers.overweight.genderClassifier")
    val model = if (Files.exists(Paths.get(modelFile))) {
      logger.info(s"$modelFile found; loading...")
      val sub = LiblinearClassifier.loadFrom[String, String](modelFile)
      val g = new GenderClassifier(useUnigrams=true, useDictionaries=true, useMaxEmbeddings=true)
      g.subClassifier = Option(sub)
      g
    } else {
      // train a fresh classifier
      logger.info(s"$modelFile not found; attempting to train...")

      val trainingData = FileUtils.load(config.getString("classifiers.gender.trainingData")) ++
        FileUtils.load(config.getString("classifiers.gender.devData")) ++
        FileUtils.load(config.getString("classifiers.gender.testData"))
      val tmp = new GenderClassifier(useUnigrams=true, useDictionaries=true, useMaxEmbeddings=true)

      // bad to have to load followers possibly multiple times, but this should happen only rarely
      // TODO: different follower files by classifier
      val followers = if (tmp.useFollowers) {
        Option(ClassifierImpl.loadFollowers(trainingData.keys.toSeq))
      } else None
      val followees = if (tmp.useFollowees) {
        Option(ClassifierImpl.loadFollowees(trainingData.keys.toSeq, "gender"))
      } else None
      tmp.setClassifier(new L1LinearSVMClassifier[String, String]())
      tmp.train(trainingData.keys.toSeq, trainingData.values.toSeq, followers, followees)
      tmp.subClassifier.get.saveTo(modelFile)
      tmp
    }
    Option(model)
  } else None

  // Followers (to be set by setFollowers())
  var handleToFollowerAccount: Option[Map[String, Seq[TwitterAccount]]] = None

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
    * Set the value of {@link handleToFollowees}
    */
  def setFollowees(followees: Map[String, Seq[String]]) = {
    handleToFollowees = Option(followees)
  }

  /**
    * Set the value of {@link handleToFollowerAccount}
    */
  def setFollowers(followers: Map[String, Seq[TwitterAccount]]) = {
    handleToFollowerAccount = Option(followers)
  }

  /**
    * Ultimately what should be called by the classifier when training.
    *
    * @param account TwitterAccount to extract features from
    * @param label Classification label associated with this account
    */
  def mkDatum(account: TwitterAccount, label: String): Datum[String, String] = {
    new RVFDatum[String, String](label, mkFeatures(account, this.useFollowers) + this.customFeatures(account))
  }

  /**
    * Scale the counter if datumScaling is true
    */
  def scale(counter: Counter[String]): Counter[String] = {
    if (datumScaling) scaleByDatum(counter, 0.0, 1.0)
    counter
  }

  /**
    * Generates the feature counter for this account
    *
    * @param account
    * @return Counter of all features signified by constructor flags
    */
  def mkFeatures(account: TwitterAccount, withFollowers: Boolean = false): Counter[String] = {
    val counter = new Counter[String]

    val tweets = for (t <- account.tweets) yield t.text.trim.split(" +")
    val description = account.description.trim.split(" +")

    var unigrams: Option[Counter[String]] = None

    if (useUnigrams | useDictionaries | useCosineSim)
      unigrams = Some(scale(ngrams(1, tweets.map(filterStopWords), description)))
    if (useUnigrams) {
      counter += unigrams.get
    }
    if (useBigrams)
      counter += scale(ngrams(2, tweets, description))
    if (useTopics)
      counter += scale(topics(tweets))
    if (useDictionaries)
      counter += dictionaries(tweets, description, account, unigrams)
    if (useAvgEmbeddings || useMinEmbeddings || useMaxEmbeddings){
      counter += embeddings(tweets)
    }
    if (useCosineSim)
      counter += cosineSim(unigrams, tweets, description)
    if (useFollowees)
      counter += scale(followees(account))

    // Each set of domain adaptation features (gender, race, followers) captured independently and then added once
    if (useGender & genderClassifier.nonEmpty) {
      counter += prepend(s"gender:${genderClassifier.get.predict(account)}_", counter)
    }

    if (useRace) {
      // TODO: predict account owner's race for domain adaptation
      // counter += prepend(s"race-${raceClassifier.get.predict(account)}_", counter)
    }

    if (withFollowers) {
      val fc = followers(account)

      // if scaling by datum, followers will have range 0-1 like main; otherwise, scale followers to have same total
      // feature count as the main features
      if (datumScaling) scaleByDatum(fc, 0.0, 1.0) // followers range 0-1
      else scaleByCounter(fc, counter)

      val followerProp = config.getNumber("classifiers.overweight.followerProp").floatValue

      counter += prepend("follower:", fc.mapValues(v => v * followerProp))
    }

    // remove zero values for sparse rep
    counter.filter{ case (k, v) => k != "" & v != 0.0 }
  }

  /**
    * Populate a [[Counter]] with frequency counts of words in a [[Seq]]
    */
  def setCounts(words: Seq[String], counter: Counter[String]) = {
    words.foreach(word => counter.incrementCount(word, 1))
  }

  /**
    * Adds ngrams from account's description and tweets with raw frequencies as weights.
    *
    * @param n Degree of n-gram (e.g. 1 refers to unigrams)
    * @param description Text description of [[TwitterAccount]]
    * @return counter
    */
  def ngrams(n: Int, tweets: Seq[Array[String]], description: Array[String]): Counter[String] = {
    val counter = new Counter[String]
    val foodWords = lexicons.get("Overweight")("activity_words")
    val activityWords = lexicons.get("Overweight")("food_words")
    val restaurantWords = lexicons.get("Overweight")("restaurant_hashtags")
    val owHashtags = lexicons.get("Overweight")("overweight_hashtags")

    // Extract ngrams
    def populateNGrams(n: Int, text: Array[String]): Seq[String] = {
      text
        .filter(w => foodWords.contains(w) ||
          activityWords.contains(w) ||
          restaurantWords.contains(w) ||
          owHashtags.contains(w))
        //.filter(w => foodWords.contains(w) || activityWords.contains(w))
        //.filter(activityWords.contains)
        //.filter(foodWords.contains)
        //.filter(owHashtags.contains)
        .sliding(n)
        .toList
        .map(ngram => ngram.mkString(s"$n-gram:", " ", ""))
    }

    setCounts(populateNGrams(n, description), counter)
    tweets.foreach{ tweet =>
      setCounts(populateNGrams(n, tweet), counter)
    }

    counter
  }

  /**
    * Binary feature for each followee (a.k.a. "friend" in the Twitter API) handle for this account
    *
    * @param account [[TwitterAccount]] whose followees' handles will be sought
    * @return counter
    */
  def followees(account: TwitterAccount): Counter[String] = {
    val counter = new Counter[String]
    val followeeHandles: Seq[String] = handleToFollowees.get.getOrElse(account.handle, Nil)
    setCounts(followeeHandles.map(handle => s"followeeHandle:$handle"), counter)
    counter
  }

  /**
    * Adds the flagged features to a new counter for domain adaptation.
    *
    * @param account [[TwitterAccount]] whose followers will be annotated
    * @return counter
    */
  def followers(account: TwitterAccount): Counter[String] = {
    assert(handleToFollowerAccount.nonEmpty)

    // Find the TwitterAccount object corresponding to these handles
    val followers = handleToFollowerAccount.get.getOrElse(account.handle, Nil)

    // filter out followers judged not to be humans
    val filteredFollowers = if (useHuman) followers.filter(f => humanClassifier.get.predict(f) == "Human") else followers

    // Aggregate the counter for the followers using the other features being used
    // withFollowers must be false to prevent infinite regress
    val followerCounters = for (follower <- filteredFollowers.par) yield mkFeatures(follower, withFollowers = false)

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
    val topics = new Counter[String]

    // no topic model available
    if (topicModel.isEmpty) return topics

    val tm = topicModel.get
    tweets.foreach(tweet => topics.incrementCount(s"topic:${tm.mostLikelyTopic(tweet)}__"))

    topics
  }

  /**
    * Count the number of times a word from the relevant dictionary appears
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
                  if (i == 0 & lexicon.contains(n)) nS += 1
                  else if (lexName.contains("last") & lexicon.contains(n)) nS += 1
              }

              // Check substrings for handles
              val matches = lexicon.keySet.filter(account.handle.toLowerCase.drop(1).contains(_))
              nS += matches.size

              val dS = if (!lexName.contains("name")) description.count(lexicon.contains) else 0

              if(dS > 0) result.incrementCount(s"dictionary:lex_${k}_${lexName}_description", dS)
              if(nS > 0) result.incrementCount(s"dictionary:lex_${k}_${lexName}_name", nS)
          }
      }
    }
    else if(cType equals "race") {

    }
    else if(cType equals "overweight") {
      // Load dictionaries
      val foodWords = lexicons.get("Overweight")("food_words")
      val hashtags = lexicons.get("Overweight")("overweight_hashtags")
      val activityWords = lexicons.get("Overweight")("activity_words")
      // keep track of the average calorie and health grade of the food words mentioned
      val calCount = new ArrayBuffer[Double]()
      val healthCount = new ArrayBuffer[Int]()

      // Use pre-existing ngrams, which probably exist, but generate them again if necessary.
      val ng = if (ngramCounter.nonEmpty) ngramCounter.get else ngrams(1, tweets, description)
      ng.keySet.foreach{ k =>
        val wd = dehashtag(k)
        if(foodWords contains wd) {
          result.incrementCount("dictionary:foodDict", ng.getCount(wd))
          result.incrementCount("dictionary:overweightDict", ng.getCount(wd))
          if (calories.get.contains(k)) calCount.append(calories.get(wd))
          if (healthiness.get.contains(k)) healthCount.append(healthiness.get(wd))
        }
        if(hashtags contains k) {
          result.incrementCount("dictionary:hashtagDict", ng.getCount(k))
          result.incrementCount("dictionary:overweightDict", ng.getCount(k))
        }
        if(activityWords contains k) {
          result.incrementCount("dictionary:activityDict", ng.getCount(k))
          result.incrementCount("dictionary:overweightDict", ng.getCount(k))
        }
      }
      // Scale by number of tweets (number of tokens also a possibility)
      if (datumScaling) {
        val scalingFactor = tweets.length.toDouble
        result.keySet.foreach{ k => result.setCount(k, result.getCount(k) / scalingFactor)}
      }
      // Add features for average calories and health grade of food words mentioned (if any were)
      // Don't scale these, since they're averages
      if (calCount.nonEmpty)
        result.setCount("dictionary:averageCalories", calCount.sum / calCount.size.toDouble)
      if (healthCount.nonEmpty)
        result.setCount("dictionary:averageHealthScore", healthCount.sum.toDouble / healthCount.size.toDouble)
    }

    result
  }

  /**
    * Add one feature per embedding vector dimension, the average of the non-stopwords in the account's tweets
    *
    * @param tweets Tweets, pre-tokenized
    * @return a [[Counter]] of the averaged vector
    */
  def embeddings(tweets: Seq[Array[String]]): Counter[String] = {
    // Number of dimensions
    val dims = vectors.get.head._2.length
    // Only look at the tokens that have a vector listen in our lexicon
    val listedTokens = filterStopWords(tweets.flatten.toArray).filter(vectors.get.contains(_))
    // Number of listed tokens for this account
    val totalTokens = listedTokens.length.toDouble
    // For each token, the vector values listed in the word2vec model
    val vectorPerToken = for (token <- listedTokens.par) yield vectors.get(token)
    // For each dimension, the values for each token in the account
    val valuesPerDim = for (dim <- 0 until dims) yield vectorPerToken.map(token => token(dim)).seq

    val counter = new Counter[String]()
    // Take the average of each dimension's values over all tokens in the account
    valuesPerDim.indices.foreach{ i =>
      if (useAvgEmbeddings) counter.setCount(s"avgembedding:$i", valuesPerDim(i).sum / totalTokens)
      if (valuesPerDim(i).nonEmpty) {
        if (useMinEmbeddings) counter.setCount(s"minembedding:$i", valuesPerDim(i).min)
        if (useMaxEmbeddings) counter.setCount(s"maxembedding:$i", valuesPerDim(i).max)
      }
    }
    counter
  }

  private def loadVectors: Option[Map[String, Array[Double]]] = {
    logger.info("Loading word embeddings...")
    val lines = scala.io.Source.fromFile(config.getString("classifiers.features.vectors")).getLines
    lines.next() // we don't need to know how big the vocabulary or vectors are
    val vectorMap = scala.collection.mutable.Map[String, Array[Double]]()
    while (lines.hasNext) {
      val line = lines.next()
      val splits = line.split(" ")
      vectorMap += splits.head -> splits.tail.map(_.toDouble)
    }
    Some(vectorMap.toMap)
  }

  // Loads the idf table for the database of random tweets
  private def loadTFIDF: (Option[Counter[String]], Option[Counter[String]]) = {
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
        // tweets are already tokenized and filtered, but stopwords are still there
        filterStopWords(line.split(" +")).foreach(token => randomCounter.incrementCount(token))
        N += 1
        pb.step()
      }
      i += 1
      i %= 3
    }

    randomFile.close()
    pb.stop()

    randomCounter.keySet.foreach(word => randomCounter.setCount(word, scala.math.log(N / randomCounter.getCount(word))))

    // Do the same for overweight corpus, using idf table for idf value
    val overweightCounter = new Counter[String]()
    var overweightFile = new BufferedReader(new FileReader(config.getString("classifiers.features.overweight_tweets")))
    numLines = 0
    while (overweightFile.readLine() != null) numLines += 1
    overweightFile.close()

    overweightFile = new BufferedReader(new FileReader(config.getString("classifiers.features.overweight_tweets")))

    val pb2 = new me.tongfei.progressbar.ProgressBar("loadTFIDF()", 100)
    pb2.start()
    pb2.maxHint(numLines/3)
    pb2.setExtraMessage("Loading tfidf vector for overweight corpus...")

    i = 0
    line = ""
    while ( { line = overweightFile.readLine ; line != null } ) {
      if (i % 3 == 2) {
        // tweets are already tokenized and filtered, but stopwords are still there
        filterStopWords(line.split(" +")).foreach(token => overweightCounter.incrementCount(token))
        pb2.step()
      }
      i += 1
      i %= 3
    }

    overweightFile.close()
    pb.stop()

    overweightCounter.keySet.foreach(word =>
      overweightCounter.setCount(word, math.log(overweightCounter.getCount(word)) * (1 + randomCounter.getCount(word)))
    )

    (Some(randomCounter), Some(overweightCounter))
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

    val accountVec = if (ngramCounter.nonEmpty) copyCounter(ngramCounter.get) else ngrams(1, tweets, description)

    accountVec.keySet.foreach(word =>
      accountVec.setCount(word, math.log(accountVec.getCount(word)) * (1 + idfTable.get.getCount(word)))
    )

    // Calculate cosine similarity
    val result = new Counter[String]()
    result.setCount("__cosineSim__", Counters.cosine(accountVec, overweightVec.get))

    result
  }
}

object FeatureExtractor {
  val config = ConfigFactory.load()
  val logger = LoggerFactory.getLogger(this.getClass)
  val stopWordsFile = scala.io.Source.fromFile(config.getString("classifiers.features.stopWords"))
  val stopWords = stopWordsFile.getLines.toSet
  stopWordsFile.close

  // NOTE: all features that run over description and tweets should probably apply this for consistency.
  // If the feature calculator uses tokenized tweets, this should already be done, but stopwords aren't filtered
  def filterTags(tagTok: Array[TaggedToken]): Array[String] = {
    val emptyString = "^[\\s\b]*$"
    val lumped = for (tt <- tagTok) yield {
      (tt.token, tt.tag) match {
        case (site, "U") => Some("<URL>")
        case (handle, "@") => Some("<@MENTION>")
        case (number, "$") => Some("<NUMBER>")
        case (garbage, "G") => None
        case (rt, "~") => None
        case (token, tag) if token.matches(emptyString) => None
        case (token, tag) => Some(token)
      }
    }
    lumped.flatten
  }

  def filterStopWords(tokens: Array[String]): Array[String] = tokens filterNot stopWords.contains

  def deepCopy[T](original: Counter[T]): Counter[T] = {
    val copy = new Counter[T]
    original.toSeq.foreach{ case (k, v) => copy.setCount(k, v)}
    copy
  }
}