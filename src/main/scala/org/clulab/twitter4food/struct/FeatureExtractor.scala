package org.clulab.twitter4food.struct

import java.io.{BufferedReader, FileReader}

import edu.arizona.sista.learning.{Datum, RVFDatum}
import edu.arizona.sista.struct.{Counter, Lexicon}
import org.clulab.twitter4food.util.{TestUtils, Tokenizer}
import cmu.arktweetnlp.Tagger._
import com.typesafe.config.ConfigFactory

/**
  * Created by Terron on 2/9/16.
  */
class FeatureExtractor (
  val useUnigrams:Boolean,
  val useBigrams:Boolean,
  val useTopics:Boolean,
  val useDictionaries:Boolean,
  val useEmbeddings:Boolean,
  val useCosineSim:Boolean) { // TODO: add others, network?

  val config = ConfigFactory.load()
  var lexicons: Option[Map[String, Seq[Lexicon[String]]]] = None
  var idfTable: Option[Counter[String]] = None
  var overweightVec: Option[Counter[String]] = None

  /** 
   * Additional method call for adding additional features 
   * outside of what's presented here
   */
  def mkDatum(account: TwitterAccount, label: String, 
    counter: Counter[String]): Datum[String, String] = {
    new RVFDatum[String, String](label, mkFeatures(account) + counter)
  }

  def mkDatum(account: TwitterAccount, label: String): Datum[String, String] = {
    new RVFDatum[String, String](label, mkFeatures(account))
  }

  def mkFeatures(account: TwitterAccount): Counter[String] = {
    var counter = new Counter[String]
    if (useUnigrams)
      counter += ngrams(1, account)
    if (useBigrams)
      counter += ngrams(2, account)
    if (useTopics)
      counter += topics(account)
    if (useDictionaries)
      counter += dictionaries(account)
    if (useEmbeddings){

    } // TODO: how to add embeddings as a feature if not returning a counter?
    if (useCosineSim)
      counter += cosineSim(account)

    return counter
  }

  def setCounts(words: Seq[String], counter: Counter[String]) = {
    words.foreach(word => counter.incrementCount(word, 1))
  }

  def tokenSet(tt: Array[TaggedToken]) = tt.map(t => t.token)

  def filterTags(tagTok: Array[TaggedToken]) = {
    val stopWordsFile = scala.io.Source.fromFile(config.getString("classifiers.features.stopWords"))
    val stopWords = stopWordsFile.getLines.toSet
    stopWordsFile.close
    tagTok.filter(tt => !("@UGD,~$".contains(tt.tag))
        && "#NVAT".contains(tt.tag) && !stopWords.contains(tt.token))
  }

  // TODO: Populate ngrams by filtering tokens based on tags.
  def ngrams(n: Int, account: TwitterAccount): Counter[String] = {
    val counter = new Counter[String]
    val populateNGrams = (n: Int, text: Array[String]) => {
      text.sliding(n).toList.reverse
        .foldLeft(List[String]())((l, window) => window.mkString("_") :: l)
        .toArray
      }

    setCounts(tokenSet(filterTags(Tokenizer.annotate(account.description.toLowerCase))), counter)
    account.tweets.filter(t => t.lang != null
      && t.lang.equals("en")).foreach(tweet => {
      if (tweet.text != null && !tweet.text.equals("")) {
        val tt = Tokenizer.annotate(tweet.text.toLowerCase)
        val tokenAndTagSet = filterTags(tt)
        val tokens = tokenSet(tokenAndTagSet)
        val nGramSet = populateNGrams(n, tokens)
        setCounts(nGramSet, counter)
      }
    })
    return counter
  }

  def topics(account: TwitterAccount): Counter[String] = {
    null
  }

  def dictionaries(account: TwitterAccount): Counter[String] = {

    var counter = new Counter[String]()
    if(lexicons.isDefined) {
      lexicons.get foreach {
        case (k, v) => {
          v.foreach(lexicon => {
            val desc = tokenSet(filterTags(Tokenizer
              .annotate(account.description.toLowerCase)))
            var nS = 0
            if(lexicon.contains(account.handle.toLowerCase.drop(1))) {
              counter.incrementCount(account.handle.toLowerCase.drop(1), 1)
              nS += 1
            }

            account.name.toLowerCase.split("\\s+").foreach(n => {
              if(lexicon.contains(n)) counter.incrementCount(n, 1)
              nS += 1
              })
            val dS = desc.foldLeft(0)((s, d) => if(lexicon.contains(d)) s+1 else s)
            counter.incrementCount(s"lex_$k", dS + nS)

            // TODO: Configure lexicon count for tweets
          })
        }
      }
    } else throw new RuntimeException("Lexicons must be loaded first")
    
    val foodWordsFile = scala.io.Source
        .fromFile(config.getString("classifiers.features.foodWords"))
    val foodWords = foodWordsFile.getLines.toSet
    foodWordsFile.close

    val hashtagsFile = scala.io.Source
        .fromFile(config.getString("classifiers.features.hashtags"))
    val hashtags = hashtagsFile.getLines.toSet
    hashtagsFile.close

    //var counter = ngrams(1, account)
    counter = counter.filter( tup => foodWords.contains(tup._1) || hashtags.contains(tup._1))
    counter
  }

  def embeddings(account: TwitterAccount): Map[TwitterAccount, Array[Float]] = {
    null
  }

  private def loadTFIDF(): Unit = {
    // Initialize
    val counter = new Counter[String]()
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
        val taggedTokens = filterTags(Tokenizer.annotate(line.toLowerCase))
        var wordsSeen = Set[String]()
        for (taggedToken <- taggedTokens) {
          val token = taggedToken.token
          if (!wordsSeen.contains(token))
            counter.incrementCount(token)
          else
            wordsSeen += token
        }
        N += 1
        pb.step()
      }
      i += 1
      i %= 3
    }

    randomFile.close()
    pb.stop()

    counter.keySet.foreach(word => counter.setCount(word, scala.math.log(N / counter.getCount(word))))

    idfTable = Some(counter)

    // Do the same for overweight corpus, using idf table for idf value
    val overweightCounter = new Counter[String]()
    var overweightFile = new BufferedReader(new FileReader(config.getString("classifiers.features.overweight_corpus")))
    numLines = 0
    while (overweightFile.readLine() != null) numLines += 1
    overweightFile.close

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
        val taggedTokens = filterTags(Tokenizer.annotate(line.toLowerCase))
        for (taggedToken <- taggedTokens) {
          overweightCounter.incrementCount(taggedToken.token)
        }
        pb2.step()
      }
      i += 1
      i %= 3
    }

    overweightFile.close
    pb.stop()

    overweightCounter.keySet.foreach(
      word => overweightCounter.setCount(word, math.log(overweightCounter.getCount(word)) * (1 + idfTable.get.getCount(word)))
    )
    overweightVec = Some(overweightCounter)
  }

  def cosineSim(account: TwitterAccount): Counter[String] = {
    if (!idfTable.isDefined || !overweightVec.isDefined) {
      loadTFIDF()
    }
    // Accumulate tfidf scores of words in this account
    def addToVec(vec:Counter[String], text:String) = {
      val taggedTokens = filterTags(Tokenizer.annotate(text.toLowerCase))
      taggedTokens.foreach(tagTok => vec.incrementCount(tagTok.token))
    }
    // Get tf
    val accountVec = new Counter[String]()
    addToVec(accountVec, account.description)
    account.tweets.foreach(tweet => addToVec(accountVec, tweet.text))
    // Convert to tfidf using idf from table
    accountVec.keySet.foreach(
      word => accountVec.setCount(word, math.log(accountVec.getCount(word)) * (1 + idfTable.get.getCount(word)))
    )

    // Calculate cosine similarity
    val result = new Counter[String]()
    result.setCount("cosineSim", accountVec.dotProduct(overweightVec.get) / (accountVec.l2Norm * overweightVec.get.l2Norm))

    result
  }

  // TODO: higher-level features, to be extracted from specific feature classifiers, also handled in meta classifier?
}