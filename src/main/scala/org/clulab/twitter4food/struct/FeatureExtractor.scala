package org.clulab.twitter4food.struct

import edu.arizona.sista.learning.{Datum, RVFDatum}
import edu.arizona.sista.struct.Counter
import org.clulab.twitter4food.twitter4j.Tokenizer

/**
  * Created by Terron on 2/9/16.
  */
class FeatureExtractor (val useUnigrams:Boolean,
      val useBigrams:Boolean,
      val useTopics:Boolean,
      val useDictionaries:Boolean,
      val useEmbeddings:Boolean) { // TODO: add others, network?

    // Additional method call for adding additional features outside of what's presented here
    def mkDatum(account: TwitterAccount, label: String, counter: Counter[String]): Datum[String, String] = {
        new RVFDatum[String, String](label, mkFeatures(account) + counter)
    }

    def mkDatum(account: TwitterAccount, label: String): Datum[String, String] = {
        new RVFDatum[String, String](label, mkFeatures(account))
    }

    def mkFeatures(account: TwitterAccount): Counter[String] = {
        var counter = new Counter[String]()
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

    def ngrams(n: Int, account: TwitterAccount): Counter[String] = {
        var text = account.description
        account.tweets.foreach(tweet => text += " " + tweet.text)
        text = Tokenizer.annotate(text).foldLeft("")((str, taggedToken) => str + taggedToken.token)

        var counter = new Counter[String]

        if (n == 1)
            text.split("\\s+").foreach(word => counter.incrementCount(word, 1))
        else if (n == 2){} // TODO implement bigrams

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
