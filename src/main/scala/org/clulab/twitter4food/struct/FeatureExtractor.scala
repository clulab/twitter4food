package org.clulab.twitter4food.struct

import edu.arizona.sista.learning.{Datum, RVFDatum}
import edu.arizona.sista.struct.Counter
import edu.arizona.sista.twitter4food.LDA

/**
  * Created by Terron on 2/9/16.
  */
class FeatureExtractor (val useUnigrams:Boolean,
      val useBigrams:Boolean,
      val useTopics:Boolean) { // TODO: add others


    def mkDatum(account:TwitterAccount, label:String): Datum[String, String] = {
      new RVFDatum[String, String](label, mkFeatures(account))
    }

    def mkFeatures(account:TwitterAccount):Counter[String] = {
      null
    }

    // TODO: understand LDA implementation
//    val lda = new LDA
    def ngrams(n: Int, account: TwitterAccount): Counter[String] = {
        null
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
