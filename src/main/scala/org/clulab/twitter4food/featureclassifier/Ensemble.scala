package org.clulab.twitter4food.featureclassifier

import com.typesafe.config.{Config, ConfigFactory}
import org.clulab.learning.LiblinearClassifier
import org.clulab.struct.Counter
import org.clulab.twitter4food.struct.TwitterAccount
import org.slf4j.{Logger, LoggerFactory}

class Ensemble[F <: ClassifierImpl](classifiers: Seq[F]) {
  assert(classifiers.nonEmpty)

  /** config file that fetches filepaths */
  val config: Config = ConfigFactory.load()
  val logger: Logger = LoggerFactory.getLogger(this.getClass)

  /**
    * Implements stratified cross validation; producing pairs of gold/predicted labels across the training dataset.
    * Each fold is as balanced as possible by label L. Returns the weights of each classifier in addition to predictions.
    */
  def overweightCV(
    accounts: Seq[TwitterAccount],
    labels: Seq[String],
    followers: Option[Map[String, Seq[TwitterAccount]]],
    followees: Option[Map[String, Seq[String]]],
    classifierFactory: () => LiblinearClassifier[String, String],
    numFolds: Int = 10,
    seed: Int = 73
  ): Seq[(String, String)] = {

    val numFeatures = 30
    val numAccts = 20

    val handles = accounts.map(_.handle).sorted

    // Important: this dataset is sorted by account handle
    val datasets = for (c <- classifiers) yield c.constructDataset(accounts, labels, followers, followees)
    val folds = classifiers.head.mkStratifiedTrainTestFolds(numFolds, datasets.head, seed)

    val preVote = for (dataset <- datasets) yield {
      val dsPreds = for (fold <- folds) yield {
        if (logger.isDebugEnabled) {
          val balance = fold.test.map(dataset.labels(_)).groupBy(identity).mapValues(_.size)
          logger.debug(s"fold: ${balance.mkString(", ")}")
        }
        val classifier = classifierFactory()
        classifier.train(dataset, fold.train.toArray)
        val predictions = for (i <- fold.test) yield {
          val datum = dataset.mkDatum(i)
          val gold = dataset.labelLexicon.get(dataset.labels(i))
          val score = classifier.scoresOf(datum)
          (gold, score)
        }
        predictions
      }
      dsPreds.flatten.toSeq
    }

    val voted = for (i <- handles.indices) yield {
      val sums = new Counter[String]()
      preVote.foreach { ds =>
        val score = ds(i)._2
        score.keySet.foreach{ lbl =>
          sums.incrementCount(lbl, score.getCount(lbl))
        }
      }
      val avg = sums.mapValues( _ / preVote.length.toDouble )

      // gold, highest-scored
      (preVote.head(i)._1, avg.argMax._1)
    }

    voted
  }
}