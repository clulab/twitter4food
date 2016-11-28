package org.clulab.twitter4food.miml

import org.clulab.twitter4food.struct.{RvfMLDataset, TwitterAccount}
import org.clulab.twitter4food.util.{FileUtils, Utils}
import org.clulab.twitter4food.util.Utils.Config
import org.slf4j.LoggerFactory
import edu.stanford.nlp.util.HashIndex
import org.clulab.twitter4food.featureclassifier.ClassifierImpl

import scala.collection.mutable.ArrayBuffer

object OverweightDataConstructor {

  import org.clulab.twitter4food.featureclassifier.ClassifierImpl._

  val logger = LoggerFactory.getLogger(this.getClass)

  def constructMimlDataset[L,F](params: Config): RvfMLDataset[L,F] = {

    // List of features that can apply to a single tweet (not followers, e.g.)
    // if these are all false, set default to true to use unigrams anyway
    val allFeatures = Seq(
      params.useUnigrams,
      params.useBigrams,
      params.useTopics,
      params.useDictionaries,
      params.useAvgEmbeddings,
      params.useMinEmbeddings,
      params.useMaxEmbeddings,
      params.useCosineSim,
      params.useTimeDate
    )
    val default = allFeatures.forall(!_) // true if all features are off

    val ci = new ClassifierImpl(
      useUnigrams = params.useUnigrams,
      useBigrams = params.useBigrams,
      useName = false, // doesn't apply to individual tweets
      useTopics = params.useTopics,
      useDictionaries = params.useDictionaries,
      useAvgEmbeddings = params.useAvgEmbeddings,
      useMinEmbeddings = params.useMinEmbeddings,
      useMaxEmbeddings = params.useMaxEmbeddings,
      useCosineSim = params.useCosineSim,
      useTimeDate = params.useTimeDate,
      useFollowers = false, // doesn't apply to individual tweets
      useFollowees = false, // doesn't apply to individual tweets
      useGender = false, // doesn't apply to individual tweets
      useRace = false, // doesn't apply to individual tweets
      useHuman = false, // doesn't apply to individual tweets
      datumScaling = false,
      featureScaling = false,
      variable = "overweight"
    )

    logger.info("Loading Twitter accounts")
    val labeledAccts = FileUtils.load(config.getString("classifiers.overweight.data"))
      .toSeq
      .filter(_._1.tweets.nonEmpty)

    // Scale number of accounts so that weights aren't too biased against Overweight
    val desiredProps = Map( "Overweight" -> 0.5, "Not overweight" -> 0.5 )
    val subsampled = Utils.subsample(labeledAccts, desiredProps)

    val features = new ArrayBuffer[ArrayBuffer[Array[Int]]]
    val values = new ArrayBuffer[ArrayBuffer[Array[Double]]]
    val featureIndex = new HashIndex[F]
    val labelIndex = new HashIndex[L]
    val labels = new ArrayBuffer[Set[Int]]

    for ((account, label) <- subsampled) {
      val instances = splitAccount(account)
      val dataset = ci.constructDataset(instances, List.fill(instances.length)(label), None, None)
      features.append(dataset.features)
      values.append(dataset.values)
      // TODO: Add to featureIndex and labelIndex -- MAKE SURE INDICES ALIGN TO APPENDED FEATURES
    }
  }

  def splitAccount(account: TwitterAccount): Seq[TwitterAccount] = {
    for (tweet <- account.tweets) yield {
      new TwitterAccount(
        account.handle,
        account.id,
        account.name,
        account.lang,
        account.url,
        account.location,
        account.description,
        Seq(tweet),
        Nil
      )
    }
  }

}