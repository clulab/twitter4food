package org.clulab.twitter4food.struct

import edu.arizona.sista.learning.{Datum, RVFDatum}
import edu.arizona.sista.struct.Counter
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

  // TODO: Populate ngrams by filtering tokens based on tags.

  def ngrams(n: Int, account: TwitterAccount): Counter[String] = {
    val counter = new Counter[String]
    val tokenSet = (tt: Array[TaggedToken]) => tt.map(t => t.token)
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
    null
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
