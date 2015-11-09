package edu.arizona.sista.twitter4food

import java.io._

import edu.stanford.nlp.classify._
import edu.stanford.nlp.kbp.slotfilling.classify._
import edu.stanford.nlp.ling.{BasicDatum, Datum, RVFDatum}
import edu.stanford.nlp.stats.{ClassicCounter, Counter, Counters}
import edu.stanford.nlp.util.{HashIndex, Index}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * @author Julie Tibshirani (jtibs)
 * @author nallapat@ai.sri.com
 * @author Mihai
 * @author dfried
 *
 */
object MultipleInstancesRegression {

  private def makeLocalData[L,F](dataArray: Array[Array[Datum[L, F]]], fold: Int, useRVF: Boolean, initialZLabels: Array[Array[L]]): GeneralDataset[L, F] = {
    val dataset: GeneralDataset[L, F] = if (useRVF) {
      new RVFDataset[L,F]
    } else {
      new Dataset[L,F]
    }

    for ((group, zLabels) <- dataArray zip initialZLabels) {
      for ((datum, label) <- (group zip zLabels)) {
        if (useRVF) {
          (datum.asInstanceOf[RVFDatum[L, F]]).setLabel(label)
        } else {
          (datum.asInstanceOf[BasicDatum[L, F]]).setLabel(label)
        }
        dataset.add(datum)
      }
    }

    return dataset
  }

  def sortPredictions[L](scores: Counter[L]): List[(L, Double)] = {
    import scala.collection.JavaConversions._
    (for {
      key <- scores.keySet.toList
    } yield (key, scores.getCount(key))).sortBy(-_._2)
  }

  def count[L](xs: Array[L], x: L): Int = {
    xs.filter(_ == x).size
  }
}

@SerialVersionUID(1)
class MultipleInstancesRegression[L:Manifest,F:Manifest](val positiveClass: L,
                                                         val negativeClass: L,
                                                         val numberOfTrainEpochs: Int = 10,
                                                         val numberOfFolds: Int = 5,
                                                         val onlyLocalTraining: Boolean = false,
                                                         val useRVF: Boolean = true,
                                                         val zSigma: Double = 1.0,
                                                         val localClassificationMode: LocalClassificationMode = WeightedVote,
                                                         val flippingParameter: Int = 5,
                                                         val logger: Option[PrintWriter] = None) {

  /**
   * sentence-level multi-class classifier, trained across all sentences
   * one per fold to avoid overfitting
   */
  var zClassifiers: Array[LinearClassifier[L,F]] = null
  /** this is created only if localClassificationMode == SINGLE_MODEL */
  private[twitter4food] var zSingleClassifier: LinearClassifier[L,F] = null

  /** Counts number of flips for Z labels in one epoch */

  protected var zUpdatesInOneEpoch: Int = 0

  protected def foldStart(fold: Int, size: Int): Int = {
    val foldSize: Int = size / numberOfFolds
    assert((foldSize > 0))
    val start: Int = fold * foldSize
    assert((start < size))
    return start
  }

  protected def foldEnd(fold: Int, size: Int): Int = {
    if (fold == numberOfFolds - 1) return size
    val foldSize: Int = size / numberOfFolds
    assert((foldSize > 0))
    val end: Int = (fold + 1) * foldSize
    assert((end <= size))
    return end
  }

  /**
   * get the predictions for the data under the current z-classifiers (data is all data, and will use proper z classifier for each fold)
   */
  protected def initializeZLabels(data: Array[Array[Datum[L,F]]]): Array[Array[L]] = {
    val zLabels: Array[Array[L]] = new Array[Array[L]](data.length)

    for (f <- 0 until numberOfFolds) {
      val zClassifier: LinearClassifier[L,F] = zClassifiers(f)
      assert((zClassifier != null))
      for (i <- foldStart(f, data.length) until foldEnd(f, data.length)) {
        val group = data(i)
        zLabels(i) = new Array[L](group.length)
        for (j <- 0 until group.length) {
          val datum = group(j)
          val scores = zClassifier.scoresOf(datum)
          val sortedScores = MultipleInstancesRegression.sortPredictions(scores)
          zLabels(i)(j) = sortedScores.head._1
        }
      }

    }
    return zLabels
  }

  @SuppressWarnings(Array("unchecked"))
  protected def initializeZClassifierLocally(data: Array[Array[Datum[L,F]]],
                                             initialZLabels: Array[Array[L]]): Array[LinearClassifier[L,F]] = {
    val localClassifiers: Array[LinearClassifier[L,F]] = new Array[LinearClassifier[L,F]](numberOfFolds)
    for (fold <- 0 until numberOfFolds) {
      log("Constructing dataset for the local model in fold #" + fold + "...")

      val (trainData, testData) = splitForFold(data, fold)
      val (trainInitialZLabels, testInitialZLabels) = splitForFold(initialZLabels, fold)

      assert((trainInitialZLabels.length == trainData.length))
      assert((testInitialZLabels.length == testData.length))

      val dataset: GeneralDataset[L,F] = MultipleInstancesRegression.makeLocalData(trainData, fold, useRVF, trainInitialZLabels)

      log("Fold #" + fold + ": Training local model...")
      val factory: LinearClassifierFactory[L,F] = new LinearClassifierFactory[L,F](1e-4, false, zSigma)
      val localClassifier: LinearClassifier[L,F] = factory.trainClassifier(dataset)
      log("Fold #" + fold + ": Training of the local classifier completed.")
      localClassifiers(fold) = localClassifier
    }
    return localClassifiers
  }

  protected def initializeZDataset(zLabels: Array[Array[L]], data: Array[Array[Datum[L,F]]]): GeneralDataset[L,F] = {
    var dataset: GeneralDataset[L,F] = if (useRVF) {
      new RVFDataset[L,F]
    }
    else {
      new Dataset[L,F]
    }
    var count: Int = 0
    for (i <- 0 until data.length) {
      val group: Array[Datum[L,F]] = data(i)
      for (s <- 0 until group.length) {
        val datum: Datum[L,F] = group(s)
        if (useRVF) {
          (datum.asInstanceOf[RVFDatum[L,F]]).setLabel(zLabels(i)(s))
        }
        else {
          (datum.asInstanceOf[BasicDatum[L,F]]).setLabel(zLabels(i)(s))
        }
        dataset.add(datum)
        count += 1
      }
    }
    log("Created the Z dataset with " + count + " datums.")
    return dataset
  }

  def train(data: Array[Array[Datum[L,F]]], initialZLabels: Array[Array[L]], yTargetProportions: Array[Double], groupNames: Option[Array[String]] = None) {
    import collection.JavaConversions._
    val zFactory: LinearClassifierFactory[L,F] = new LinearClassifierFactory[L,F](1e-4, false, zSigma)

    zFactory.setVerbose(false)

    for (i <- data.indices) {
        val currentCounts: Map[L, Int] = initialZLabels(i).groupBy(identity).mapValues(_.size).map(identity)
        val totalCount = currentCounts.getOrElse(positiveClass, 0) + currentCounts.getOrElse(negativeClass, 0)
        val currentProportion = currentCounts.getOrElse(positiveClass, 0).toDouble / totalCount

        groupNames.map(names => (names(i))).foreach(name => log("Group " + name))
        log("initial z labels:")
        log(initialZLabels(i) mkString " ")
        log("initial proportion" + currentProportion)
        log(s"target proportion ${yTargetProportions(i)}")
        log("\n")
    }

    zClassifiers = initializeZClassifierLocally(data, initialZLabels)


    if (onlyLocalTraining) return

    val totalIndividuals: Int = data.map(_.length).sum
    val zLabels: Array[Array[L]] = initializeZLabels(data)

    for (epoch <- 0 until numberOfTrainEpochs) {
      zUpdatesInOneEpoch = 0
      log("***EPOCH " + epoch + "***")
      val zLabelsPredictedByZ: Array[Array[L]] = new Array[Array[L]](zLabels.length)

      for (fold <- 0 until numberOfFolds) {
        val zClassifier: LinearClassifier[L,F] = zClassifiers(fold)
        val start: Int = foldStart(fold, data.length)
        val end: Int = foldEnd(fold, data.length)
        for (i <- start until end) {
          val random = new scala.util.Random(epoch)
          val group: Array[Datum[L,F]] = random.shuffle(data(i).toSeq).toArray
          zLabelsPredictedByZ(i) = predictZLabels(group, zClassifier)

          val yTargetProportion = yTargetProportions(i)

          groupNames.map(names => (names(i))).foreach(name => log("Group " + name))
          // destructively update zLabels(i)
          inferZLabelsStable(group, zLabels(i), zClassifier, epoch, yTargetProportion)
          log("\n")
        }
      }

      log("In epoch #" + epoch + " zUpdatesInOneEpoch = " + zUpdatesInOneEpoch)
      if (zUpdatesInOneEpoch != 0) {
        log("M-STEP")
        for (fold <- 0 until numberOfFolds) {
          log("EPOCH " + epoch + ": Training Z classifier for fold #" + fold)
          val trainData = splitForFold(data, fold)._1
          val trainLabels = splitForFold(zLabels, fold)._1
          val zd: GeneralDataset[L,F] = initializeZDataset(trainLabels, trainData)
          val zClassifier: LinearClassifier[L,F] = zFactory.trainClassifier(zd)
          zClassifiers(fold) = zClassifier
        }
      } else {
        log("Stopping training. Did not find any changes in the Z labels!")
        makeSingleZClassifier(initializeZDataset(zLabels, data), zFactory)
        return
      }
    }
    makeSingleZClassifier(initializeZDataset(zLabels, data), zFactory)
  }

  protected def makeSingleZClassifier(zDataset: GeneralDataset[L,F], zFactory: LinearClassifierFactory[L,F]) {
    localClassificationMode match {
      case SingleModel =>  {
        log("Training the final Z classifier...")
        zSingleClassifier = zFactory.trainClassifier(zDataset)
      }
      case _ => {
        zSingleClassifier = null
      }
    }
  }

  def splitForFold[D: Manifest](stuff: Array[D], fold: Int) = {
    val start: Int = foldStart(fold, stuff.length)
    val end: Int = foldEnd(fold, stuff.length)

    val train: Array[D] = new Array[D](stuff.length - end + start)
    val test: Array[D] = new Array[D](end - start)

    var trainOffset: Int = 0
    var testOffset: Int = 0
    for (i <- 0 until stuff.length) {
      if (i < start) {
        train(trainOffset) = stuff(i)
        trainOffset += 1
      }
      else if (i < end) {
        test(testOffset) = stuff(i)
        testOffset += 1
      }
      else {
        train(trainOffset) = stuff(i)
        trainOffset += 1
      }
    }
    (train, test)
  }


  def predictZLabels(group: Array[Datum[L,F]], zClassifier: LinearClassifier[L,F]): Array[L] = for {
    datum <- group
    probs = zClassifier.logProbabilityOf(datum)
  } yield Counters.argmax(probs)

  private[twitter4food] def computeZLogProbs(group: Array[Datum[L,F]], zClassifier: LinearClassifier[L,F], epoch: Int) =
    group.map(zClassifier.logProbabilityOf _)

  /** updates the zLabels array with new predicted z labels */
  private[twitter4food] def inferZLabelsStable(group: Array[Datum[L,F]], zLabels: Array[L], zClassifier: LinearClassifier[L,F], epoch: Int, yTargetProportion: Double) {
    import scala.collection.JavaConverters._

    val showProbs: Boolean = false
    val zLogProbs = computeZLogProbs(group, zClassifier, epoch)

    val zLabelsOld = new Array[L](zLabels.size)
    Array.copy(zLabels, 0, zLabelsOld, 0, zLabels.size)

    def currentProportion() = {
      val currentCounts: Map[L, Int] = zLabels.groupBy(identity).mapValues(_.size).map(identity)
      val totalCount = currentCounts.getOrElse(positiveClass, 0) + currentCounts.getOrElse(negativeClass, 0)
      currentCounts.getOrElse(positiveClass, 0).toDouble / totalCount
    }

    val oldProportion = currentProportion()

    if (currentProportion() == yTargetProportion) return;

    val initiallyAbove = currentProportion() > yTargetProportion

    val (goodLabel, badLabel) = if (initiallyAbove)
      (negativeClass, positiveClass)
    else
      (positiveClass, negativeClass)

    val possibleFlips = mutable.PriorityQueue.empty[(Double, Int, L)]( Ordering.by(-_._1)) // we'll want the lowest scoring
    for (s <- group.indices) {
      val label: L = zLabels(s)
      val probs: Counter[L] = zLogProbs(s)
      // check the existing label, not the predicted class:
      // intuition is that if a label doesn't match the classifier's predicted label,
      // then it's evidence that hasn't yet been incorporated
      if (label != goodLabel) {
        if (label == badLabel) {
          for (flipTo <- probs.keySet().asScala) {
            if (flipTo != label) possibleFlips.enqueue((probs.getCount(label) - probs.getCount(flipTo), s, flipTo))
          }
        } else {
          possibleFlips.enqueue((probs.getCount(label) - probs.getCount(goodLabel), s, goodLabel))
        }
      }
    }

    var numFlipped = 0
    var halt = false
    while (numFlipped < flippingParameter &&  possibleFlips.nonEmpty && ! halt) {
      val oldProportion = currentProportion()
      val (score, index, flipTo) = possibleFlips.dequeue()

      val oldLabel = zLabels(index)
      zLabels(index) = flipTo
      numFlipped += 1

      val newProportion = currentProportion()

      if (newProportion == yTargetProportion) halt = true
      if ((initiallyAbove && newProportion < yTargetProportion) || (!initiallyAbove && newProportion > yTargetProportion)) {
        halt = true
        // check to see if we've gone too far
        if (math.abs(oldProportion - yTargetProportion) <= math.abs(newProportion - yTargetProportion)) {
          zLabels(index) = oldLabel
          numFlipped -= 1
        }
      }
    }

    def scoresString(scores: Counter[L]) = (for {
        (cls, score) <- MultipleInstancesRegression.sortPredictions(scores)
        str = f"$cls $score%.3f"
    } yield str).mkString(" ")


    log(s"flipped ${numFlipped} labels")
    log(s"old proportion:    ${oldProportion}")
    log(s"new proportion:    ${currentProportion()}")
    log(s"target proportion: ${yTargetProportion}")
    log(s"old\tnew\tscores")
    for ((oldLabel, newLabel, scoreCounter) <- (zLabelsOld, zLabels, zLogProbs).zipped) {
        log(s"${oldLabel}\t${newLabel}\t${scoresString(scoreCounter)}")
    }


    zUpdatesInOneEpoch += numFlipped
  }

  /**
   * Implements weighted voting over the different Z classifiers in each fold
   * @return Probabilities (NOT log probs!) for each known label
   */
  def classifyLocally(datum: Datum[L,F]): Counter[L] = localClassificationMode match {
    case WeightedVote => {
      val sumProbs: Counter[L] = new ClassicCounter[L]
      for (clf <- zClassifiers) {
        sumProbs.addAll(clf.probabilityOf(datum))
      }
      import scala.collection.JavaConversions._
      for (l <- sumProbs.keySet) sumProbs.setCount(l, sumProbs.getCount(l) / numberOfFolds)
      sumProbs
    }
    case SingleModel => {
      zSingleClassifier.probabilityOf(datum)
    }
  }

  def classifyMentions(sentences: List[Datum[L,F]]): Counter[L] = {
    throw new UnsupportedOperationException("classifyMentions not implemented!")
  }

  def log(string: String) = logger.foreach(pw => pw.println(string))
}
