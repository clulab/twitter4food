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
    partitions: Map[Long, Int],
    portion: Double = 1.0, // This doesn't do anything yet
    followers: Option[Map[String, Seq[TwitterAccount]]],
    followees: Option[Map[String, Seq[String]]],
    classifierFactory: () => LiblinearClassifier[String, String]
  ): Seq[(String, String)] = {

    // Important: this dataset is sorted by account id
    val datasets = for (c <- classifiers) yield c.constructDataset(accounts, labels, followers, followees)
    assert(datasets.forall(ds => ds.size == accounts.length), "Must have complete datasets")

    val ids = accounts.map(_.id).sorted
    val folds = classifiers.head.foldsFromIds(ids, partitions)

    val preVote = for (dataset <- datasets) yield {
      val dsPreds = for (fold <- folds) yield {
        if (logger.isDebugEnabled) {
          val balance = fold.test.map(dataset.labels(_)).groupBy(identity).mapValues(_.size)
          logger.debug(s"fold: ${balance.mkString(", ")}")
        }
        val classifier = classifierFactory()
        classifier.train(dataset, fold.train.toArray)
        val predictions = for (i <- fold.test) yield {
          val gold = dataset.labelLexicon.get(dataset.labels(i))
          val datum = dataset.mkDatum(i)
          val score = classifier.scoresOf(datum)
          (gold, score)
        }
        predictions
      }
      dsPreds.flatten
    }

    val voted = for (i <- ids.indices) yield {
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