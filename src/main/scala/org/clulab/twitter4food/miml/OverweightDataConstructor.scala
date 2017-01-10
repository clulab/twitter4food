package org.clulab.twitter4food.miml

import org.clulab.twitter4food.struct.{RvfMLDataset, TwitterAccount}
import org.clulab.twitter4food.util.{FileUtils, Utils}
import org.clulab.twitter4food.util.Utils.Config
import org.slf4j.LoggerFactory
import org.clulab.twitter4food.featureclassifier.ClassifierImpl

import scala.collection.mutable.ArrayBuffer

object OverweightDataConstructor {

  val logger = LoggerFactory.getLogger(this.getClass)

  def constructMimlDataset(accounts: Seq[(TwitterAccount, String)], params: Config): RvfMLDataset[String, String] = {

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

    logger.info("Splitting accounts into instances...")
    val ds = new RvfMLDataset[String, String](accounts.length)

    val pb = new me.tongfei.progressbar.ProgressBar("split", 100)
    pb.start()
    pb.maxHint(accounts.size)
    pb.setExtraMessage("splitting...")

    for ((account, lbl) <- accounts) {
      // Dataset with one row per instance (tweet)
      // The datasets labels are meaningless for now, hence "Overweight" to allow dictionary loading
      // "Overweight" label shouldn't be passed forward
      val instances = splitAccount(account)
      val dataset = ci.constructDataset(instances,
        List.fill(instances.length)("Overweight"),
        followers = None,
        followees = None,
        progressBar = false
      )

      var sz = 0

      if (instances.length >= 10) {
        val (dataset, tweetsInOrder) = ci.constructDatasetWithTweets(instances,
          List.fill(instances.length)("Overweight"),
          followers = None,
          followees = None,
          progressBar = false
        )

        // Add by feature NAME, not feature INDEX
        val featureStrings = dataset.features.map(row => row.map(dataset.featureLexicon.get))
        // MIML solvers need java.lang.Doubles
        val valuesJava = dataset.values.map(row => row.map(_.asInstanceOf[java.lang.Double]))
        val tweetsJava = tweetsInOrder.map(_.text).toArray
        // a singleton set containing the gold label
        val label = new java.util.HashSet[String](1)
        label.add(lbl)
        // add this account (datum) with all its instances
        ds.add(label, listify(featureStrings), listify(valuesJava), listify(tweetsJava))

        sz = dataset.size
      }

      pb.step()
    }

    pb.stop()

    ds
  }

  // Spoof a separate twitter account for each group of tweets just to piggyback on feature generation
  def splitAccount(account: TwitterAccount): Seq[TwitterAccount] = {
    val totalTweets = account.tweets.length
    val tweetsPerInstance = totalTweets
    val stride = totalTweets
    val lastIndex = Math.ceil(totalTweets.toDouble / stride - 1).toInt
    for (i <- 0 until lastIndex) yield {
      new TwitterAccount(
        account.handle,
        account.id,
        account.name,
        account.lang,
        account.url,
        account.location,
        "",
        account.tweets.slice(i * stride, Math.min(totalTweets, i * stride + tweetsPerInstance)),
        Nil
      )
    }
  }

  // Scala Array* to java List
  def listify[T](array: ArrayBuffer[Array[T]]): java.util.List[java.util.List[T]] = {
    val list = new java.util.ArrayList[java.util.ArrayList[T]](array.length)
    array.foreach{ inner =>
      val el = new java.util.ArrayList[T](inner.length)
      inner.foreach(t => el.add(t))
      list.add(el)
    }
    list.asInstanceOf[java.util.List[java.util.List[T]]]
  }

  // Scala Array* to java List
  def listify[T](array: Array[T]): java.util.List[T] = {
    val list = new java.util.ArrayList[T](array.length)
    array.foreach(inner => list.add(inner))
    list.asInstanceOf[java.util.List[T]]
  }

}