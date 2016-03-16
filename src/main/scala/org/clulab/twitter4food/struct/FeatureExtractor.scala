package org.clulab.twitter4food.struct

import edu.arizona.sista.learning.{Datum, RVFDatum}
import edu.arizona.sista.struct.Counter
import org.clulab.twitter4food.twitter4j.Tokenizer
import cmu.arktweetnlp.Tagger._

/**
  * Created by Terron on 2/9/16.
  */
class FeatureExtractor (val useUnigrams:Boolean,
  val useBigrams:Boolean,
  val useTopics:Boolean,
  val useDictionaries:Boolean,
  val useEmbeddings:Boolean) { // TODO: add others, network?

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

  // TODO: Populate ngrams by filtering tokens based on tags.

  def ngrams(n: Int, account: TwitterAccount): Counter[String] = {
    val counter = new Counter[String]
    val tokenSet = (tt: Array[TaggedToken]) => tt.map(t => t.token)
    val populateNGrams = (n: Int, text: Array[String]) => {
      text.sliding(n).toList.reverse
        .foldLeft(List[String]())((l, window) => window.mkString("_") :: l)
        .toArray
      }

    setCounts(tokenSet(Tokenizer.annotate(account.description)), counter)

    account.tweets.filter(_.lang.equals("en")).foreach(tweet => {
      if (tweet.text != null && !tweet.text.equals("")) {
        val tokenAndTagSet = Tokenizer.annotate(tweet.text)
          .filter(tagTok => !"@UGD~$:".contains(tagTok.tag))
        if(tokenAndTagSet.isEmpty) println(s"${account.handle}\t${tweet.id}")
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
