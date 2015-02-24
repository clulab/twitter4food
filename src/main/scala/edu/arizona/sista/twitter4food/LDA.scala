package edu.arizona.sista.twitter4food

import cc.mallet.util._
import cc.mallet.types._
import cc.mallet.pipe._
import cc.mallet.pipe.iterator._
import cc.mallet.topics._

import java.util.ArrayList
import java.util.regex._
import java.io._
import java.util.Properties
import collection.JavaConversions._
import jp.sf.amateras.solr.scala._


object LDAPreprocessor extends Preprocessor(Some(Tweet.tokenPattern),
  Some(Tweet.stopWords ++ LocationStopwords.stopWords ++ Set("breakfast", "lunch", "dinner", "supper", "brunch", "snack")))

/**
 * Created by dfried on 2/6/14.
 */
@SerialVersionUID(4461966945144166181L)
class LDA(val model: ParallelTopicModel, val pipe: SerialPipes) extends Serializable {
  def distributions(tokens: Seq[String],
                    numIterations: Int = 10,
                    thinning: Int = 1,
                    burnIn: Int = 5) = {
    val inst = pipe.instanceFrom(LDA.mkInstance(tokens))
    model.getInferencer.getSampledDistribution(inst, 10, 1, 5)
  }

  def mostLikelyTopic(tokens: Seq[String]): Int = {
    val dists = distributions(tokens)
    dists.zipWithIndex.max._2
  }
}

object LDA {
  def stripHashtag(token: String) = {
    if (token.startsWith("#")) token.substring(1, token.length) else token
  }

  def mkInstance(tokens: Seq[String]) = {
    val t = LDAPreprocessor.processTokens(tokens map stripHashtag)
    new Instance(new TokenSequence(t.map(new Token(_))), null, null, null)
  }

  def train(tokensList: Iterable[Seq[String]],
            numTopics: Int = 200,
            numIterations: Int = 2000) = {
    // Begin by importing documents from text to feature sequences
    var pipeList = new ArrayList[Pipe]

    // Pipes: remove stopwords, map to features
    //pipeList.add( new TokenSequenceRemoveStopwords(new File("research/src/main/scala/edu/arizona/sista/twitter4food/stoplists/twitter_food.txt"), "UTF-8", false, false, false) )
    pipeList.add( new TokenSequence2FeatureSequence() )

    val pipe = new SerialPipes(pipeList)

    val instances = new InstanceList (pipe)

    for (tokens <- tokensList) {
      instances.addThruPipe(mkInstance(tokens))
    }
    var model = new ParallelTopicModel(numTopics, 2.0, 0.01)

    model.addInstances(instances)

    // Use two parallel samplers, which each look at one half the corpus and combine
    //  statistics after every iteration.
    model.setNumThreads(2)

    // System.out.printf("running for %d iterations", numIterations)
    model.setNumIterations(numIterations)
    model.estimate
    new LDA(model, pipe)
  }

  def load(filename: String) = Utils.deserialize[LDA](filename)

  def main(args: Array[String]) = {
    if (args.size != 2) {
      println("usage: (AllTokens | HashtagTokens | FoodTokens | FoodHashtagTokens) output_file")
    }
    val tokenType = args(0) match {
      case "AllTokens" => AllTokens
      case "HashtagTokens" => HashtagTokens
      case "FoodTokens" => FoodTokens
      case "FoodHashtagTokens" => FoodHashtagTokens
    }
    def tokenProc(tokens: Seq[String]) = tokens.filter(tokenType.okToken _)
    val allTweets = (new SolrPager()).tweets// TweetParser.loadTweets(props.getProperty("tweetCorpus"))
    val pager = new SolrPager()
    //val someTweets = pager.pagedRequest(pager.client.query("normalizedLocation:[* TO *]").sortBy("postTime", Order.asc))
    val lda = LDA.train(allTweets map (tweet => tokenProc(tweet.tokens)), 200, 2000)

    Utils.serialize(lda, args(1))
  }
}
