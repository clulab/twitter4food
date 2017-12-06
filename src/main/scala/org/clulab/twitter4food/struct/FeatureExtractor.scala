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

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Designed to be used in tandem with a classifier, with the features
  * to be included "turned on" with the inputted parameters. The features
  * listed here are fairly general - custom features can be added with
  * the polymorphic mkDatum function.
  *
  * All parameters are flags for which features should be used.
  *
  * @author Terron Ishida
  * @author Dane Bell
  * @param useUnigrams unigram token features
  * @param useBigrams bigram token features
  * @param useName name- and handle-based char features
  * @param useTopics Latent Dirichlet Analysis topic features
  * @param useDictionaries classifier-specific custom dictionaries
  * @param useAvgEmbeddings average embeddings of all account words
  * @param useMinEmbeddings minimum (by dimension) embeddings of all account words
  * @param useMaxEmbeddings maximum (by dimension) embeddings of all account words
  * @param useCosineSim similarity to a corpus of overweight-related tweets
  * @param useLocation names of venues visited by user
  * @param useTimeDate time- and day-based features
  * @param useFoodPerc use the percentage of user images containing food
  * @param useFollowers domain transfer from follower accounts
  * @param useFollowees account followee handles
  * @param useRT treat retweet and non-RT n-grams differently, Daume-style
  * @param useGender domain transfer based on classification of account gender
  * @param useAge domain transfer based on classification of account age
  * @param useRace domain transfer based on classification of account race
  * @param useHuman limit follower domain transfer to those judged as human
  * @param datumScaling scale by account
  * @param customFeatures use classifier-specific custom features
  */
class FeatureExtractor (
  val useUnigrams: Boolean = false,
  val useBigrams: Boolean = false,
  val useName: Boolean = false,
  val useTopics: Boolean = false,
  val useDictionaries: Boolean = false,
  val useAvgEmbeddings: Boolean = false,
  val useMinEmbeddings: Boolean = false,
  val useMaxEmbeddings: Boolean = false,
  val useCosineSim: Boolean = false,
  val useLocation: Boolean = false,
  val useTimeDate: Boolean = false,
  val useFoodPerc: Boolean = false,
  val useCaptions: Boolean = false,
  val useFollowers: Boolean = false,
  val useFollowees: Boolean = false,
  val useRT: Boolean = false,
  val useGender: Boolean = false,
  val useAge: Boolean = false,
  val useRace: Boolean = false,
  val useHuman: Boolean = false,
  val dictOnly: Boolean = false,
  val denoise: Boolean = false,
  val datumScaling: Boolean = false,
  val variable: String,
  val customFeatures: (TwitterAccount) => Counter[String] = account => new Counter[String]()) {

  import FeatureExtractor._

  val config = ConfigFactory.load()
  val logger = LoggerFactory.getLogger(this.getClass)
  logger.info(s"useUnigrams=$useUnigrams, " +
    s"useBigrams=$useBigrams, " +
    s"useName=$useName, " +
    s"useTopics=$useTopics, " +
    s"useDictionaries=$useDictionaries, " +
    s"useAvgEmbeddings=$useAvgEmbeddings, " +
    s"useMinEmbeddings=$useMinEmbeddings, " +
    s"useMaxEmbeddings=$useMaxEmbeddings, " +
    s"useCosineSim=$useCosineSim, " +
    s"useLocation=$useLocation, " +
    s"useTimeDate=$useTimeDate, " +
    s"useFoodPerc=$useFoodPerc, " +
    s"useCaptions=$useCaptions, " +
    s"useFollowers=$useFollowers, " +
    s"useFollowees=$useFollowees, " +
    s"useRT=$useRT, " +
    s"useGender=$useGender, " +
    s"useAge=$useAge, " +
    s"useRace=$useRace, " +
    s"useHuman=$useHuman, " +
    s"datumScaling=$datumScaling"
  )

  // LDA topic model
  val topicModel: Option[LDA] = if (useTopics) {
    logger.info("Loading LDA topic model...")
    Some(LDA.load(config.getString("lda.topicModel")))
  } else None

  // Dictionaries : Map[Label -> Map[LexiconName -> Lexicon]]
  // Messy, but useful data structure.
  val lexicons: Option[Map[String, Map[String, Lexicon[String]]]] = if(useDictionaries || dictOnly) {
    val lbls = config.getStringList(s"classifiers.${this.variable}.possibleLabels").asScala.toSet
    val lexMap = populateLexiconList(lbls, this.variable)
    val l = lexMap map {
      case (k, v) => (k, v.map(fileName => {
        val lexName = fileName.substring(fileName.lastIndexOf("/") + 1,
          fileName.indexOf("."))
        (lexName, Lexicon.loadFrom[String](fileName))
      }).toMap)
    }

    Option(l)
  } else None

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
    foodAnnotations.close()
    (Some(cMap.toMap), Some(hMap.toMap))
  } else (None, None)

  // Word2vec word embedding vectors
  val vectors = if (useAvgEmbeddings || useMinEmbeddings || useMaxEmbeddings) loadVectors else None
  // tdidf vector for overweight corpus
  val (idfTable, overweightVec) = if (useCosineSim) loadTFIDF else (None, None)

  // locations
  val locations: Map[Long, Seq[Location]] = if (useLocation) {
    val unsorted = FileUtils.loadLocations(config.getString("classifiers.overweight.tweetLocs"))
    unsorted.groupBy(l => l.user)
  } else Map[Long, Seq[Location]]()

  // % food images annotations
  val (twFoodPerc, igFoodPerc): (Option[Map[Long,Double]], Option[Map[Long,Double]]) = if (useFoodPerc) {
    val twFile = scala.io.Source.fromFile(config.getString("classifiers.overweight.twFoodPerc"))
    val twAnnos = twFile.getLines.toSeq.map{ line =>
      val elements = line.trim.split('\t').take(2)
      elements.head.toLong -> elements.last.toDouble
    }.toMap
    twFile.close()

    val igFile = scala.io.Source.fromFile(config.getString("classifiers.overweight.igFoodPerc"))
    val igAnnos = igFile.getLines.toSeq.map{ line =>
      val elements = line.trim.split('\t').take(2)
      elements.head.toLong -> elements.last.toDouble
    }.toMap
    igFile.close()

    (Option(twAnnos), Option(igAnnos))
  } else (None, None)

  // image captions (generic)
  val captions = if (useCaptions) Option(loadCaptions(config.getString("classifiers.overweight.captions"))) else None

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

      val trainingData = FileUtils.loadTwitterAccounts(config.getString("classifiers.human.trainingData")) ++
        FileUtils.loadTwitterAccounts(config.getString("classifiers.human.devData")) ++
        FileUtils.loadTwitterAccounts(config.getString("classifiers.human.testData"))
      // val tmp = new HumanClassifier(useDictionaries=true, useFollowers=true, useMaxEmbeddings=true)
      val tmp = new HumanClassifier(useDictionaries=true, useMaxEmbeddings=true, customFeatures = HumanClassifier.customFeatures)

      // bad to have to loadTwitterAccounts followers possibly multiple times, but this should happen only rarely
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
      val g = new GenderClassifier(useUnigrams=true, useDictionaries=true, useTopics=true, useTimeDate=true)
      g.subClassifier = Option(sub)
      g
    } else {
      // train a fresh classifier
      logger.info(s"$modelFile not found; attempting to train...")

      val trainingData = FileUtils.loadTwitterAccounts(config.getString("classifiers.gender.trainingData")) ++
        FileUtils.loadTwitterAccounts(config.getString("classifiers.gender.devData")) ++
        FileUtils.loadTwitterAccounts(config.getString("classifiers.gender.testData"))
      val tmp = new GenderClassifier(useUnigrams=true, useDictionaries=true, useMaxEmbeddings=true)

      // bad to have to loadTwitterAccounts followers possibly multiple times, but this should happen only rarely
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

  def trimQuotes(s: String): String = s.replaceAll("\"", "")

  val (ageAnnotation, genderAnnotation) = if (useAge || useGender) {
    val annoFile = config.getString("classifiers.overweight.ageGenderAnnotations")
    val bufferedSource = io.Source.fromFile(annoFile)
    val rows = for (line <- bufferedSource.getLines) yield {
      val cols = line.split(",").map(_.trim).map(trimQuotes)
      assert(cols.length == 3)
      if ((cols(1) != "NA" || cols(2) != "NA") && cols(0) != "id")
        Option((cols(0), cols(1), cols(2)))
      else
        None
    }
    val age = rows.flatten.map{ case (id, a, g) => id -> a }.toMap
    val gender = rows.flatten.map{ case (id, a, g) => id -> g }.toMap
    (Option(age), Option(gender))
  } else (None, None)

  // Followers (to be set by setFollowers())
  var handleToFollowerAccount: Option[Map[String, Seq[TwitterAccount]]] = None

  /**
    * Copy a [[Counter]] so it's not accidentally overwritten
    */
  def copyCounter[T](counter: Counter[T]): Counter[T] = counter.map(kv => kv._2)

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

  val allDicts = if (dictOnly) {
    val label = config.getStringList(s"classifiers.${this.variable}.possibleLabels").asScala.head
    Option(lexicons.get(label).values.toSeq)
  } else None

  /**
    * Returns [[RVFDatum]] containing the features for a single [[TwitterAccount]]
    *
    * @param account [[TwitterAccount]] to extract features from
    * @param label classification label associated with this account
    */
  def mkDatum(account: TwitterAccount, label: String): Datum[String, String] = {
    new RVFDatum[String, String](label, mkFeatures(account, isProband = true) + this.customFeatures(account))
  }

  /**
    * Scales a [[Counter]] if {@link datumScaling} is true
    */
  def scale(counter: Counter[String]): Counter[String] = {
    if (datumScaling) scaleByDatum(counter, 0.0, 1.0)
    counter
  }

  def retokenize(t: String): Array[String] = {
    val separated = t.trim.split("\\s+")
    separated.map{
      case "<@MENTION>" => "<@MENTION>"
      case "<URL>" => "<URL>"
      case "<NUMBER>" => "<NUMBER>"
      case other => other.toLowerCase
    }
  }

  /**
    * Returns a [[Counter]] containing all the features signified by constructor flags
    *
    * @param account the [[TwitterAccount]] under analysis
    * @param isProband allow domain adaptation if yes (none for followers)
    */
  def mkFeatures(account: TwitterAccount, isProband: Boolean = false): Counter[String] = {
    val counter = new Counter[String]

    val description = account.description.trim.split("\\s+")
    val denoised = if (denoise) account.tweets.filterNot(isNoise) else account.tweets
    val regularizedTweets = denoised.map(t => retokenize(t.text))

    var unigrams: Option[Counter[String]] = None

    if (useUnigrams | useDictionaries | useCosineSim)
      unigrams = Some(scale(ngrams(1, denoised, description)))
    if (useUnigrams) {
      counter += unigrams.get
    }
    if (useBigrams)
      counter += scale(ngrams(2, denoised, description))
    if (useName)
      counter += name(account)
    if (useTopics)
      counter += scale(topics(regularizedTweets))
    if (useDictionaries)
      counter += dictionaries(denoised, description, account, unigrams)
    if (useAvgEmbeddings || useMinEmbeddings || useMaxEmbeddings){
      counter += embeddings(regularizedTweets)
    }
    if (useCosineSim)
      counter += cosineSim(unigrams, denoised, description)
    if (useLocation && isProband)
      counter += location(account.id)
    if (useTimeDate)
      counter += timeDate(denoised)
    if (useFoodPerc)
      counter += foodPerc(account.id)
    if (useCaptions)
      counter += captionNgrams(account.id)
    if (useFollowees)
      counter += scale(followees(account))

    // Domain adaptation from here on -- no DA of DA
    // Each set of domain adaptation features (gender, race, followers) captured independently and then added once
    val daCounter = new Counter[String]

    // use annotations if possible, then fall back to classifier
    if (useGender && isProband) {
      val acctGenderFirst = if (genderAnnotation.nonEmpty) genderAnnotation.get.get(account.id.toString) else None
      val acctGenderSecond = if (acctGenderFirst.isEmpty && genderClassifier.nonEmpty)
        genderClassifier.get.predict(account)
      else "UNK"
      daCounter += prepend(s"gender:${acctGenderFirst.getOrElse(acctGenderSecond)}_", counter)
    }

    if (useAge && isProband && ageAnnotation.nonEmpty) {
      val ageExact = ageAnnotation.get.get(account.id.toString)
      val ageApprox = if (ageExact.nonEmpty) {
        (ageExact.get.toDouble.round.toInt / 10 * 10).toString
      } else "UNK"

      daCounter += prepend(s"age:${ageApprox}_", counter)
    }

    if (useRace && isProband) {
      // TODO: predict account owner's race for domain adaptation
      // counter += prepend(s"race-${raceClassifier.get.predict(account)}_", counter)
    }

    if (useFollowers && isProband) {
      val fc = followers(account)

      // if scaling by datum, followers will have range 0-1 like main; otherwise, scale followers to have same total
      // feature count as the main features
      if (datumScaling) scaleByDatum(fc, 0.0, 1.0) // followers range 0-1
      else scaleByCounter(fc, counter)

      val followerProp = config.getNumber("classifiers.overweight.followerProp").floatValue

      daCounter += prepend("follower:", fc.mapValues(v => v * followerProp))
    }

    counter += daCounter

    // remove zero values for sparse rep
    counter.filter{ case (k, v) => k != "" & v != 0.0 }
  }

  /**
    * Populate a [[Counter]] with frequency counts of words in a [[Seq]]
    */
  def setCounts(words: Seq[String], counter: Counter[String]) = {
    words.foreach(word => counter.incrementCount(word))
  }

  /**
    * Returns the text cut into strings of <i>n</i> characters (with padding)
    */
  def charNGrams(n: Int, text: String, prefix: String = ""): Seq[String] = {
    assert(n > 0, "Cannot populate character n-grams of length < 1")
    val padded = ("^" * (n-1)) + text + ("^" * (n - 1))
    padded.sliding(n).toList.map(ngram => ngram.mkString(s"${prefix}char$n-gram:", "", ""))
  }

  /**
    * Returns pretokenized text cut into strings of <i>n</i> tokens (with padding)
    */
  def tokenNGrams(n: Int, text: Array[String], prefix: String = ""): Seq[String] = {
    assert(n > 0, "Cannot populate token n-grams of length < 1")
    val padded = Seq.fill(n-1)("<s>") ++ text ++ Seq.fill(n-1)("</s>")
    text.sliding(n).toList.map(ngram => ngram.mkString(s"$prefix$n-gram:", " ", ""))
  }

  /**
    * Returns a [[Counter]] of ngrams from account's description and tweets with raw frequencies as weights.
    *
    * @param n Degree of n-gram (e.g. 1 refers to unigrams)
    * @param description Text description of [[TwitterAccount]]
    */
  def ngrams(n: Int, tweets: Seq[Tweet], description: Array[String]): Counter[String] = {
    val counter = new Counter[String]

    val denoised = if (denoise) tweets.filterNot(isNoise) else tweets

    // special prefix for description tokens since they summarize an account more than tweets
    setCounts(tokenNGrams(n, description, "desc"), counter)

    // n-gram for tweets
    denoised.foreach{ tweet =>
      val split = retokenize(tweet.text) // split on whitespace
    val relevant = if (dictOnly) dictFilter(split) else split // only relevant words if 'dictOnly'
    val filtered = if (n == 1) filterStopWords(relevant) else relevant // remove stopwords
      setCounts(tokenNGrams(n, filtered), counter) // always set n-gram counts
      if (useRT) setCounts(tokenNGrams(n, filtered, if (tweet.isRetweet) "RT_" else "NRT_"), counter) // prepend if marking RT
    }

    counter
  }

  /**
    * Returns a [[Counter]] of character/word n-grams based on user's name and handle
    *
    * @param account the [[TwitterAccount]] under analysis
    */
  def name(account: TwitterAccount): Counter[String] = {
    val counter = new Counter[String]
    val cleanHandle = account.handle.replaceFirst("@", "")

    // 1-, 2-, and 3-grams for the user's handle
    setCounts(charNGrams(1, cleanHandle, "handle"), counter)
    setCounts(charNGrams(2, cleanHandle, "handle"), counter)
    setCounts(charNGrams(3, cleanHandle, "handle"), counter)

    if (account.name.length > 3) setCounts(tokenNGrams(1, account.name.split("\\s+"), "name"), counter)

    // 1-, 2-, and 3-grams for the user's name
    setCounts(charNGrams(1, account.name, "name"), counter)
    setCounts(charNGrams(2, account.name, "name"), counter)
    setCounts(charNGrams(3, account.name, "name"), counter)

    counter
  }

  /**
    * Returns a [[Counter]] representing a binary feature for each followee (a.k.a. "friend" in the Twitter API) handle
    * for this account
    *
    * @param account [[TwitterAccount]] whose followees' handles will be sought
    */
  def followees(account: TwitterAccount): Counter[String] = {
    val counter = new Counter[String]
    val followeeHandles: Seq[String] = handleToFollowees.get.getOrElse(account.handle, Nil)
    setCounts(followeeHandles.map(handle => s"followeeHandle:$handle"), counter)
    counter
  }

  /**
    * Returns a [[Counter]] of all the flagged features for the current [[TwitterAccount]]'s followers for domain
    * adaptation.
    *
    * @param account [[TwitterAccount]] whose followers will be annotated
    */
  def followers(account: TwitterAccount): Counter[String] = {
    assert(handleToFollowerAccount.nonEmpty)

    // Find the TwitterAccount object corresponding to these handles
    val followers = handleToFollowerAccount.get.getOrElse(account.handle, Nil)

    // filter out followers judged not to be humans
    val filteredFollowers = if (useHuman) followers.filter(f => humanClassifier.get.predict(f) == "Human") else followers

    // Aggregate the counter for the followers using the other features being used
    // withFollowers must be false to prevent infinite regress
    val followerCounters = for (follower <- filteredFollowers.par) yield mkFeatures(follower, isProband = false)

    val followerCounter = new Counter[String]
    followerCounters.seq.foreach(fc => followerCounter += fc)

    followerCounter
  }

  /**
    * Returns a [[Counter]] containing a feature for each topic in the topic model, counting instances in the
    * account's tweets
    *
    * @param tweets tokenized, filtered [[Tweet]] text
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
    * Returns a [[Counter]] for the number of times a word from the classifier-relevant custom dictionary appears
    *
    * @param tweets pre-tokenized tweet text
    * @param description pre-tokenized account description text
    * @param account the whole [[TwitterAccount]] under analysis
    */
  def dictionaries(tweets: Seq[Tweet],
    description: Array[String],
    account: TwitterAccount,
    ngramCounter: Option[Counter[String]]): Counter[String] = {

    val counter = new Counter[String]()
    if(lexicons.isEmpty) return counter

    // Classifier type
    val cType = lexicons.get.keys.head match {
      case "M" | "F" => "gender"
      case "Overweight" | "Not overweight" => "overweight"
      case "risk" | "not" => "diabetes"
      case "human" | "org" => "human"
      case "asian" | "hispanic" | "white" | "black" => "race"
    }

    if((cType == "human") || (cType == "gender")) {
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

              if(dS > 0) counter.incrementCount(s"dictionary:lex_${k}_${lexName}_description", dS)
              if(nS > 0) counter.incrementCount(s"dictionary:lex_${k}_${lexName}_name", nS)
          }
      }
    }
    else if(cType == "race") {

    }
    else if(cType == "overweight" || cType == "diabetes") {
      val k = if (cType == "overweight") "Overweight" else "risk"

      // Load dictionaries
      val foodWords = lexicons.get(k)("food_words")
      val activityWords = lexicons.get(k)("activity_words")
      val restaurants = lexicons.get(k)("restaurant_hashtags")
      val hashtags = lexicons.get(k)("overweight_hashtags")
      // keep track of the average calorie and health grade of the food words mentioned
      val calCount = new ArrayBuffer[Double]()
      val healthCount = new ArrayBuffer[Int]()

      // Use pre-existing ngrams, which probably exist, but generate them again if necessary.
      val ng = if (ngramCounter.nonEmpty) ngramCounter.get else ngrams(1, tweets, description)
      ng.keySet.foreach{ k =>
        val wd = dehashtag(k)
        if(foodWords contains wd) {
          counter.incrementCount("dictionary:foodDict", ng.getCount(wd))
          counter.incrementCount("dictionary:overweightDict", ng.getCount(wd))
          if (calories.get.contains(k)) calCount.append(calories.get(wd))
          if (healthiness.get.contains(k)) healthCount.append(healthiness.get(wd))
        }
        if(hashtags contains k) {
          counter.incrementCount("dictionary:hashtagDict", ng.getCount(k))
          counter.incrementCount("dictionary:overweightDict", ng.getCount(k))
        }
        if(activityWords contains k) {
          counter.incrementCount("dictionary:activityDict", ng.getCount(k))
        }
        if(restaurants contains k) {
          counter.incrementCount("dictionary:restaurantDict", ng.getCount(k))
          counter.incrementCount("dictionary:overweightDict", ng.getCount(k))
        }
      }
      // Scale by number of tweets (number of tokens also a possibility)
      if (datumScaling) {
        val scalingFactor = tweets.length.toDouble
        counter.keySet.foreach{ k => counter.setCount(k, counter.getCount(k) / scalingFactor)}
      }
      // Add features for average calories and health grade of food words mentioned (if any were)
      // Don't scale these, since they're averages
      if (calCount.nonEmpty)
        counter.setCount("dictionary:averageCalories", calCount.sum / calCount.size.toDouble)
      if (healthCount.nonEmpty)
        counter.setCount("dictionary:averageHealthScore", healthCount.sum.toDouble / healthCount.size.toDouble)
    }

    counter
  }

  /**
    * Returns a [[Counter]] containing one feature per embedding vector dimension, which may correspond to the average,
    * minimum, and/or maximum value of each dimension of the non-stopwords in the account's tweets.
    *
    * @param tweets Tweets, pre-tokenized
    * @return a [[Counter]] of the averaged, minimum, and/or maximum vector
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

  // Loads the embeddings for creating embedding-related features
  private def loadVectors: Option[Map[String, Array[Double]]] = {
    logger.info("Loading word embeddings...")
    val lines = scala.io.Source.fromFile(config.getString("classifiers.features.food_vectors")).getLines
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
        filterStopWords(line.split("\\s+")).foreach(token => randomCounter.incrementCount(token))
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
        filterStopWords(line.split("\\s+")).foreach(token => overweightCounter.incrementCount(token))
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
    * @param ngramCounter A unigram counter, if one exists
    * @param tweets tweet text, pre-tokenized
    * @param description the account description
    * @return a [[Counter]] with a single cosine similarity feature
    */
  def cosineSim(ngramCounter: Option[Counter[String]], tweets: Seq[Tweet],
    description: Array[String]): Counter[String] = {

    val accountVec = if (ngramCounter.nonEmpty) copyCounter(ngramCounter.get) else ngrams(1, tweets, description)

    accountVec.keySet.foreach(word =>
      accountVec.setCount(word, math.log(accountVec.getCount(word)) * (1 + idfTable.get.getCount(word)))
    )

    // Calculate cosine similarity
    val counter = new Counter[String]
    counter.setCount("__cosineSim__", Counters.cosine(accountVec, overweightVec.get))

    counter
  }

  /**
    * Returns a [[Counter]] of unigrams based on the locations the user has visited.
    *
    * @param id the current account's id
    * @return a [[Counter]] with location-based unigram features
    */
  def location(id: Long): Counter[String] = {
    val counter = new Counter[String]

    if (locations contains id) {
      val venues = locations(id).flatMap(l => l.venues.headOption)
      venues.foreach{ venue =>
        // venue name -> lowercase, then split on whitespace and all punctuation
        // this is because of things like "Panera Northridge" and "Panera Bread, Union Station"
        setCounts(tokenNGrams(1, venue.name.toLowerCase.split("[\\s\\p{Punct}]+"), "loc"), counter)
        // venue TYPE, e.g. restaurant
        setCounts(venue.types.map("locType:" + _), counter)
      }
    }

    counter
  }

  /**
    * A set of features describing the time and day the account tweets.
    *
    * @param tweets [[Tweet]]s of the account for time/date info
    * @return a [[Counter]] with time and day features
    */
  def timeDate(tweets: Seq[Tweet]): Counter[String] = {
    val counter = new Counter[String]
    if (tweets.isEmpty) return counter
    val numTweets = tweets.length.toDouble

    // Convert java.util.Date into java.time.LocalDateTime
    val zid = java.time.ZoneId.of("GMT")
    val dateTimes = tweets.map(w => java.time.LocalDateTime.ofInstant(w.createdAt.toInstant, zid))

    // What hour of the day is the user most likely to tweet (0-23 hr)? This is intentionally an Int division.
    val hours = dateTimes.map(_.getHour)
    counter.setCount("timeDate:avghr", hours.sum / hours.length)

    // What proportion of tweets are written in each hour span of the day
    val hrHist = hours.groupBy(identity).mapValues(_.length / numTweets)
    hrHist.foreach{ case (hr, prop) => counter.setCount(s"timeDate:hr$hr", prop) }

    // What is the standard deviation of the tweet time (constant vs. peaky, for example) in hours
    val seconds = dateTimes.map(t => t.getHour.toDouble * 60 * 60 + t.getMinute * 60 + t.getSecond)
    val mean = seconds.sum / numTweets
    val sd = scala.math.sqrt(seconds.map(s => scala.math.pow(s - mean, 2)).sum / numTweets) / 60 / 60
    counter.setCount("timeDate:sd", sd)

    // What day of the week is the user most likely to tweet?
    val dayOfWeek = dateTimes.map(_.getDayOfWeek)

    val dayHist = dayOfWeek.groupBy(identity).mapValues(_.length / numTweets)
    dayHist.foreach{ case (day, prop) => counter.setCount(s"timeDate:$day", prop) }

    counter
  }

  /**
    * Returns a [[Counter]] with percentages of Twitter and Instagram image files containing food (if any exist)
    */
  def foodPerc(id: Long): Counter[String] = {
    val counter = new Counter[String]

    if (twFoodPerc.nonEmpty && twFoodPerc.get.contains(id)) {
      counter.setCount("foodPerc:twitter", twFoodPerc.get(id))
    }
    if (igFoodPerc.nonEmpty && igFoodPerc.get.contains(id)) {
      counter.setCount("foodPerc:instagram", igFoodPerc.get(id))
    }

    counter
  }

  def captionNgrams(id: Long, n: Int = 1): Counter[String] = {
    val counter = new Counter[String]

    if (captions.nonEmpty && captions.get.contains(id)) {
      val userCaptions = captions.get(id)
      userCaptions.foreach{ caption =>
        val split = retokenize(caption) // split on whitespace
      val filtered = if (n == 1) filterStopWords(split) else split // remove stopwords
        setCounts(tokenNGrams(n, filtered, prefix = "cap_"), counter)
      }
    }

    counter
  }

  /**
    * Returns a map from TwitterAccount id to the captions for their images
    */
  private def loadCaptions(fileName: String): scala.collection.immutable.Map[Long, Seq[String]] = {
    val file = scala.io.Source.fromFile(fileName)

    val captions = new mutable.HashMap[Long, Seq[String]]

    val lines = file.getLines.toSeq

    // Start progress bar
    val pb = new me.tongfei.progressbar.ProgressBar("captions", 100)
    pb.start()
    pb.maxHint(lines.length)

    lines.foreach{ line =>
      val chunks = line.trim.split("\t")
      if (chunks.length > 2) {
        // get user ID for image
        val id = chunks.head.toLong
        // get most likely caption only, getting rid of extra parenthesis
        val caption = chunks(2).drop(1)
        // join this caption to previous captions for this user
        captions(id) = captions.getOrElse(id, Nil) :+ caption
      }
      pb.step()
    }

    pb.stop()

    file.close()

    captions.toMap
  }


  def dictFilter(text: Array[String]): Array[String] = {
    assert(! (dictOnly && allDicts.isEmpty))
    if(allDicts.nonEmpty) {
      text.filter(w => allDicts.get.exists(lex => lex.contains(w)))
    } else text
  }
}

object FeatureExtractor {
  val config = ConfigFactory.load()
  val logger = LoggerFactory.getLogger(this.getClass)
  val stopWordsFile = scala.io.Source.fromFile(config.getString("classifiers.features.stopWords"))
  val stopWords = stopWordsFile.getLines.toSet
  stopWordsFile.close

  /** Populates list of lexicons from config file. Separate function
    * for easy testing.
    *
    * @param labelSet Set of labels
    * @param ctype Type of classifier
    * @return map of label -> Seq of lexicon file names
    */
  def populateLexiconList(labelSet: Set[String], ctype: String) = {
    labelSet.foldLeft(Map[String, Seq[String]]())(
      (m, l) => m + (l ->
        config.getStringList(s"classifiers.$ctype.$l.lexicons").asScala.toList))
  }


  // NOTE: all features that run over description and tweets should probably apply this for consistency.
  // If the feature calculator uses tokenized tweets, this should already be done, but stopwords aren't filtered
  def filterTags(tagTok: Array[TaggedToken]): Array[String] = {
    val emptyString = "^[\\s\b]*$"

    val url = "^(http|:/)".r

    val punct = """,\\."/\\\\"""
    val hasPunct = s"[^$punct][$punct]|[$punct][^$punct]".r
    val punctSplit = s"(?=[$punct])|(?<=[$punct])"

    val emoji = "\\ud83c\\udc00-\\ud83c\\udfff\\ud83d\\udc00-\\ud83d\\udfff\\u2600-\\u27ff"
    val hasEmoji = s"[^$emoji][$emoji]|[$emoji][^$emoji]".r
    val emojiSplit = s"(?=[$emoji])|(?<=[$emoji])"

    val lumped = for (tt <- tagTok) yield {
      (tt.token, tt.tag) match {
        case (empty, tag) if empty.matches(emptyString) => Nil
        case (site, "U") => Seq("<URL>")
        case (handle, "@") => Seq("<@MENTION>")
        case (number, "$") => Seq("<NUMBER>")
        case (garbage, "G") => Nil
        case ("RT", "~") => Seq("RT")
        case ("rt", "~") => Seq("RT")
        case (otherRT, "~") => Nil
        case (site, tag) if url.findFirstIn(site).nonEmpty => Seq("<URL>")
        case (slashed, tag) if hasPunct.findFirstIn(slashed).nonEmpty =>
          mergeRegex(slashed.split(punctSplit), s"[$punct]")
        case (emojis, tag) if hasEmoji.findFirstIn(emojis).nonEmpty =>
          mergeRegex(emojis.split(emojiSplit), s"[$emoji]")
        case (token, tag) => Seq(token)
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

  // Given a string with multiple adjacent identical emoji, merge them into a single string
  def mergeRegex(tokens: Array[String], regex: String): Seq[String] = {
    val merged = new ArrayBuffer[String]
    val curr = new StringBuilder
    var prev = ""
    tokens.foreach {
      case "" => ()
      case p if p == prev => curr append p
      case e if e matches regex =>
        if (curr.nonEmpty) {
          merged append curr.toString
          curr.clear
        }
        curr append e
        prev = e
      case other =>
        if (curr.nonEmpty) {
          merged append curr.toString
          curr.clear
          prev = ""
        }
        merged append other
    }
    if (curr.nonEmpty) merged append curr.toString
    merged
  }
}