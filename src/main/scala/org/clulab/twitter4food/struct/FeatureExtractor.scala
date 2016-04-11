package org.clulab.twitter4food.struct

import edu.arizona.sista.learning.{Datum, RVFDatum}
import edu.arizona.sista.struct.{Counter, Lexicon}
import org.clulab.twitter4food.util.{TestUtils, Tokenizer}
import cmu.arktweetnlp.Tagger._
import com.typesafe.config.ConfigFactory

/**
  * Created by Terron on 2/9/16.
  */
class FeatureExtractor (val useUnigrams:Boolean,
  val useBigrams:Boolean,
  val useTopics:Boolean,
  val useDictionaries:Boolean,
  val useEmbeddings:Boolean) { // TODO: add others, network?

  val config = ConfigFactory.load()
  var lexicons: Option[Map[String, Seq[Lexicon[String]]]] = None

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
    if (useEmbeddings){} // TODO: how to add embeddings as a feature if not returning a counter?

    return counter
  }

  def setCounts(words: Seq[String], counter: Counter[String]) = {
    words.foreach(word => counter.incrementCount(word, 1))
  }

  def filterTags(tagTok: Array[TaggedToken]) = {
    tagTok.filter(tt => !("@UGD,~$".contains(tt.tag))
      && "#NVAT".contains(tt.tag))
  }

  def tokenSet(tt: Array[TaggedToken]) = tt.map(t => t.token)

  // TODO: Populate ngrams by filtering tokens based on tags.
  def ngrams(n: Int, account: TwitterAccount): Counter[String] = {
    val counter = new Counter[String]
    val populateNGrams = (n: Int, text: Array[String]) => {
      text.sliding(n).toList.reverse
        .foldLeft(List[String]())((l, window) => window.mkString("_") :: l)
        .toArray
      }

    val stopWords = scala.io.Source
      .fromFile(config.getString("classifiers.features.stopWords"))
      .getLines.toSet

    setCounts(tokenSet(filterTags(Tokenizer.annotate(account.description.toLowerCase))), counter)
    account.tweets.filter(t => t.lang != null
      && t.lang.equals("en")).foreach(tweet => {
      if (tweet.text != null && !tweet.text.equals("")) {
        val tt = Tokenizer.annotate(tweet.text.toLowerCase)
        val tokenAndTagSet = filterTags(tt).filter(x => !stopWords.contains(x.token))
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
    
    val foodWords = scala.io.Source
        .fromFile(config.getString("classifiers.features.foodWords"))
        .getLines.toSet
    val hashtags = scala.io.Source
        .fromFile(config.getString("classifiers.features.hashtags"))
        .getLines.toSet

    //var counter = ngrams(1, account)
    counter = counter.filter( tup => foodWords.contains(tup._1) || hashtags.contains(tup._1))
    counter
  }

  def embeddings(account: TwitterAccount): Map[TwitterAccount, Array[Float]] = {
    null
  }

  // TODO: perhaps network should be handled by classifier?
  // Network can be accessed recursively and other features can be extracted from there
  //    def network(account: TwitterAccount): Unit = {
  //
  //    }

  // TODO: higher-level features, to be extracted from specific feature classifiers, also handled in meta classifier?
}
