package org.clulab.twitter4food.util

import java.io.File
import java.nio.file.{FileSystem, FileSystems, Files, Paths}

import com.typesafe.config.ConfigFactory
import org.clulab.learning.{RVFDataset, RVFDatum}
import org.clulab.struct.Counter
import org.clulab.twitter4food.featureclassifier.ClassifierImpl
import org.clulab.twitter4food.t2dm.OverweightClassifier
import org.slf4j.LoggerFactory

object PrintFeatures {
  import ClassifierImpl._

  val logger = LoggerFactory.getLogger(this.getClass)

  // cf. RVFDataset.mkDatum
  def mkDatum[L, F](dataset: RVFDataset[L, F], row: Int): RVFDatum[Int, F] = {
    val intFeats = dataset.featuresCounter(row)
    val feats = new Counter[F]
    for (f <- intFeats.keySet) {
      feats.setCount(dataset.featureLexicon.get(f), intFeats.getCount(f))
    }
    new RVFDatum[Int, F](dataset.labels(row), feats)
  }

  def main(args: Array[String]): Unit = {
    // Parse args using standard Config
    val params = Utils.parseArgs(args)
    val config = ConfigFactory.load

    // List of features (not counting domain adaptation)
    // if these are all false, set default to true to use unigrams anyway
    val allFeatures = Seq(
      params.useUnigrams,
      params.useBigrams,
      params.useName,
      params.useTopics,
      params.useDictionaries,
      params.useAvgEmbeddings,
      params.useMinEmbeddings,
      params.useMaxEmbeddings,
      params.useCosineSim,
      params.useTimeDate,
      params.useFollowees
    )
    val default = allFeatures.forall(!_) // true if all features are off

    val portions = if (params.learningCurve) (1 to 20).map(_.toDouble / 20) else Seq(1.0)

    val nonFeatures = Seq("--analysis", "--test", "--learningCurve")
    // This model and results are specified by all input args that represent featuresets
    val fileExt = args.filterNot(nonFeatures.contains).sorted.mkString("").replace("-", "")

    val outputDir = config.getString("classifier.overweight.rawfeatures")
    if (!Files.exists(Paths.get(outputDir))) {
      if (new File(outputDir).mkdir()) logger.info(s"Created output directory $outputDir")
      else logger.info(s"ERROR: failed to create output directory $outputDir")
    }

    logger.info("Loading Twitter accounts")
    val labeledAccts = FileUtils.load(config.getString("classifiers.overweight.data"))
      .toSeq
      .filter(_._1.tweets.nonEmpty)

    // Scale number of accounts so that weights aren't too biased against Overweight
    val desiredProps = Map( "Overweight" -> 0.5, "Not overweight" -> 0.5 )
    val subsampled = Utils.subsample(labeledAccts, desiredProps)

    val followers = if(params.useFollowers) {
      logger.info("Loading follower accounts...")
      Option(ClassifierImpl.loadFollowers(subsampled.map(_._1)))
    } else None

    val followees = if(params.useFollowees) {
      logger.info("Loading followee accounts...")
      Option(ClassifierImpl.loadFollowees(subsampled.map(_._1), "overweight"))
    } else None

    val oc = new OverweightClassifier(
      useUnigrams = default || params.useUnigrams,
      useBigrams = params.useBigrams,
      useName = params.useName,
      useTopics = params.useTopics,
      useDictionaries = params.useDictionaries,
      useAvgEmbeddings = params.useAvgEmbeddings,
      useMinEmbeddings = params.useMinEmbeddings,
      useMaxEmbeddings = params.useMaxEmbeddings,
      useCosineSim = params.useCosineSim,
      useTimeDate = params.useTimeDate,
      useFollowers = params.useFollowers,
      useFollowees = params.useFollowees,
      useGender = params.useGender,
      useRace = params.useRace,
      datumScaling = params.datumScaling,
      featureScaling = params.featureScaling)

    logger.info("Training classifier...")

    val (accounts, labels) = subsampled.unzip
    val dataset = oc.constructDataset(accounts, labels, followers, followees)
    val fileSystem = FileSystems.getDefault()

    val datums = for (row <- dataset.indices) yield mkDatum(dataset, row)
    RVFDataset.saveToSvmLightFormat(datums, dataset.featureLexicon, outputDir + fileSystem.getSeparator + fileExt + ".dat")
  }
}