package edu.arizona.sista.twitter4food

import java.io._

import edu.stanford.nlp.classify._
import edu.stanford.nlp.kbp.slotfilling.classify._
import edu.stanford.nlp.kbp.slotfilling.common.{Constants, Log}
import edu.stanford.nlp.ling.{BasicDatum, Datum, RVFDatum}
import edu.stanford.nlp.stats.{ClassicCounter, Counter, Counters}
import edu.stanford.nlp.util.{ErasureUtils, HashIndex, Index}

/**
 * Modification of the MIML-RE model to predict real-valued y-outputs from three-class z-labels. Z-labels have a positive class, a negative, and a neutral, and the y-value is simply positive / (positive + negative)
 * @author Julie Tibshirani (jtibs)
 * @author nallapat@ai.sri.com
 * @author Mihai
 * @author dfried
 *
 */
@SerialVersionUID(-7961154075748697901L)
object GroupRegression {

  private def makeInitialModelPath(workDir: String, serializedRelationExtractorName: String, modelType: ModelType, samplingRatio: Double): String = {
    return workDir + File.separator + serializedRelationExtractorName + "." + modelType + "." + (100.0 * samplingRatio).toInt + ".initial" + Constants.SER_EXT
  }

  private def makeModelPath(workDir: String, serializedRelationExtractorName: String, modelType: ModelType, samplingRatio: Double): String = {
    return workDir + File.separator + serializedRelationExtractorName + "." + modelType + "." + (100.0 * samplingRatio).toInt + Constants.SER_EXT
  }

  private def makeCoocurrenceFeature(src: String, dst: String): String = {
    return "co:s|" + src + "|d|" + dst + "|"
  }

  private def makeLocalData(dataArray: Array[Array[Datum[String, String]]], labelIndex: Index[String], featureIndex: Index[String], fold: Int, useRVF: Boolean, initialZLabels: Array[Array[String]]): GeneralDataset[String, String] = {

    val negLabels: java.util.Set[Integer] = new java.util.HashSet[Integer]
    val nilIndex: Int = labelIndex.indexOf(JointlyTrainedRelationExtractor.UNRELATED)
    negLabels.add(nilIndex)

    val dataset: GeneralDataset[String, String] = if (useRVF) {
      new WeightedRVFDataset[String, String]()
    } else {
      new WeightedDataset[String, String]
    }
    dataset.featureIndex = featureIndex
    dataset.labelIndex = labelIndex

    for ((group, zLabels) <- dataArray zip initialZLabels) {
      for ((datum, label) <- (group zip zLabels)) {
        if (useRVF) {
          val d = datum.asInstanceOf[RVFDatum[String, String]]
          d.setLabel(label)
          (dataset.asInstanceOf[WeightedRVFDataset[String, String]]).add(d, 1.0f)
        } else {
          (datum.asInstanceOf[BasicDatum[String, String]]).setLabel(label)
          (dataset.asInstanceOf[WeightedDataset[String, String]]).add(datum, 1.0f)
        }
      }
    }

    return dataset
  }

  private val BIG_WEIGHT: Double = +10

  def sortPredictions(scores: Counter[String]): List[(String, Double)] = {
    import scala.collection.JavaConversions._
    (for {
      key <- scores.keySet.toList
    } yield (key, scores.getCount(key))).sortBy(-_._2)
  }

  def count(xs: Array[Int], x: Int): Int = {
    var count: Int = 0
    for (xP <- xs) {
      if (xP == x) ({
        count += 1;
        count - 1
      })
    }
    return count
  }
}

@SerialVersionUID(-7961154075748697901L)
class GroupRegression(val positiveClass: String,
                      val negativeClass: String,
                      val initialModelPath: String = null,
                      val numberOfTrainEpochs: Int = 10,
                      val numberOfFolds: Int = 5,
                      val featureModel: Int = 0,
                      val trainY: Boolean = true,
                      val onlyLocalTraining: Boolean = false,
                      val useRVF: Boolean = true,
                      val zSigma: Double = 1.0,
                       val localClassificationMode: LocalClassificationMode = WeightedVote) {

  var positiveIndex: Int = 0
  var negativeIndex: Int = 0

  /**
   * sentence-level multi-class classifier, trained across all sentences
   * one per fold to avoid overfitting
   */
  var zClassifiers: Array[LinearClassifier[String, String]] = null
  /** this is created only if localClassificationMode == SINGLE_MODEL */
  private[twitter4food] var zSingleClassifier: LinearClassifier[String, String] = null

  protected var featureIndex: Index[String] = null
  protected var zLabelIndex: Index[String] = null
  /** Counts number of flips for Z labels in one epoch */

  protected var zUpdatesInOneEpoch: Int = 0
  /** These label dependencies were seen in training */
  protected var knownDependencies: Set[String] = null

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

  protected def initializeZLabels(data: MultiLabelDataset[String, String]): Array[Array[Int]] = {
    val zLabels: Array[Array[Int]] = new Array[Array[Int]](data.getDataArray.length)

    val dataArray = data.getDataArray

    for (f <- 0 until numberOfFolds) {
      val zClassifier: LinearClassifier[String, String] = zClassifiers(f)
      assert((zClassifier != null))
      for (i <- foldStart(f, data.getDataArray.length) until foldEnd(f, data.getDataArray.length)) {
        val group = dataArray(i)
        zLabels(i) = new Array[Int](group.length)
        for (j <- 0 until group.length) {
          val datum = group(j)
          val scores: Counter[String] = zClassifier.scoresOf(datum)
          val sortedScores = GroupRegression.sortPredictions(scores)
          val sys: Int = zLabelIndex.indexOf(sortedScores.head._1)
          assert((sys != -1))
          zLabels(i)(j) = sys
        }
      }

    }
    return zLabels
  }

  def train(data: MultiLabelDataset[String, String], initialZLabels: Array[Array[String]]) {
    val zFactory: LinearClassifierFactory[String, String] = new LinearClassifierFactory[String, String](1e-4, false, zSigma)

    zFactory.setVerbose(false)

    featureIndex = data.featureIndex
    zLabelIndex = new HashIndex[String](data.labelIndex)
    zLabelIndex.add(JointlyTrainedRelationExtractor.UNRELATED)

    positiveIndex = zLabelIndex.indexOf(positiveClass)
    negativeIndex = zLabelIndex.indexOf(negativeClass)

    zClassifiers = initializeZClassifierLocally(data, featureIndex, zLabelIndex, initialZLabels)

    if (onlyLocalTraining) return

    var totalSentences: Int = 0
    for (group <- data.getDataArray) totalSentences += group.length
    val zLabels: Array[Array[Int]] = initializeZLabels(data)

    var yDataset: RVFDataset[String, String] = new RVFDataset[String, String]
    for (epoch <- 0 until numberOfTrainEpochs) {
      zUpdatesInOneEpoch = 0
      Log.severe("***EPOCH " + epoch + "***")
      val zLabelsPredictedByZ = zLabels.map(group => new Array[Int](group.length)).toArray

      for (fold <- 0 until numberOfFolds) {
        val zClassifier: LinearClassifier[String, String] = zClassifiers(fold)
        val start: Int = foldStart(fold, data.getDataArray.length)
        val end: Int = foldEnd(fold, data.getDataArray.length)
        for (i <- start until end) {
          val random = new scala.util.Random(epoch)
          val group: Array[Datum[String, String]] = random.shuffle(data.getDataArray(i))
          val positiveLabels: Set[Integer] = data.getPositiveLabelsArray(i)
          val negativeLabels: Set[Integer] = data.getNegativeLabelsArray(i)
          val zLogProbs: Array[Counter[String]] = ErasureUtils.uncheckedCast(new Array[Counter[_]](group.length))
          zLabelsPredictedByZ(i) = predictZLabels(group, zClassifier)
          inferZLabelsStable(group, positiveLabels, negativeLabels, zLabels(i), zLogProbs, zClassifier, epoch)
          assert((positiveLabels.size == 1))
          val yLabel: String = yLabelIndex.get(positiveLabels.iterator.next)
          addYDatum(yDataset, yLabel, zLabels(i))
        }
      }

      computeConfusionMatrixForCounts("EPOCH " + epoch, zLabels, data.getPositiveLabelsArray)
      Log.severe("In epoch #" + epoch + " zUpdatesInOneEpoch = " + zUpdatesInOneEpoch)
      if (zUpdatesInOneEpoch == 0) {
        Log.severe("Stopping training. Did not find any changes in the Z labels!")
        break //todo: break is not supported
      }
      val zDataset: GeneralDataset[String, String] = initializeZDataset(totalSentences, zLabels, data.getDataArray)
      Log.severe("M-STEP")
      for (fold <- 0 until numberOfFolds) {
        Log.severe("EPOCH " + epoch + ": Training Z classifier for fold #" + fold)
        val foldTrainArray: Array[Array[Int]] = splitForFold(zDataset.getDataArray, fold)._1
        val foldTrainLabels: Array[Int] = splitForFold(zDataset.getLabelsArray, fold)._1
        val zd: Dataset[String, String] = new Dataset[String, String](zLabelIndex, foldTrainLabels, featureIndex, foldTrainArray)
        val zClassifier: LinearClassifier[String, String] = zFactory.trainClassifier(zd)
        zClassifiers(fold) = zClassifier
      }


      if (trainY) {
        Log.severe("EPOCH " + epoch + ": Training Y classifier")
        yClassifier = yFactory.trainClassifier(yDataset)
      }
      Log.severe("classifierWeights")
      Log.severe("class " + positiveClass)
      Log.severe("" + POSITIVE_CLASS_FRACTION + ":" + yClassifier.weight(POSITIVE_CLASS_FRACTION, positiveClass))
      Log.severe("" + BIAS_FEAT + ":" + yClassifier.weight(BIAS_FEAT, positiveClass))
      val epochPath: String = makeEpochPath(epoch)
      yDataset = new RVFDataset[String, String]
    }
    val zDataset: GeneralDataset[String, String] = initializeZDataset(totalSentences, zLabels, data.getDataArray)
    makeSingleZClassifier(zDataset, zFactory)
  }

  private[twitter4food] def computeConfusionMatrixForCounts(name: String, zLabels: Array[Array[Int]], golds: Array[Set[Integer]]) {
    val pos: Counter[Integer] = new ClassicCounter[Integer]
    val neg: Counter[Integer] = new ClassicCounter[Integer]
    val nilIndex: Int = zLabelIndex.indexOf(JointlyTrainedRelationExtractor.UNRELATED)
    for (i <- 0 until zLabels.length) {
      val zs: Array[Int] = zLabels(i)
      val freqs: Counter[Integer] = new ClassicCounter[Integer]
      for (z <- zs) if (z != nilIndex) freqs.incrementCount(z)
      val gold: Set[Integer] = golds(i)
      import scala.collection.JavaConversions._
      for (z <- freqs.keySet) {
        val f: Int = freqs.getCount(z).toInt
        if (gold.contains(z)) {
          pos.incrementCount(f)
        }
        else {
          neg.incrementCount(f)
        }
      }
    }
    Log.severe("CONFUSION MATRIX for " + name)
    Log.severe("CONFUSION MATRIX POS: " + pos)
    Log.severe("CONFUSION MATRIX NEG: " + neg)
  }

  protected def makeSingleZClassifier(zDataset: GeneralDataset[String, String], zFactory: LinearClassifierFactory[String, String]) {
    if (GroupRegression.localClassificationMode eq GroupRegression.LOCAL_CLASSIFICATION_MODE.SINGLE_MODEL) {
      Log.severe("Training the final Z classifier...")
      zSingleClassifier = zFactory.trainClassifier(zDataset)
    }
    else {
      zSingleClassifier = null
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

  @SuppressWarnings(Array("unchecked")) protected def initializeZClassifierLocally(data: MultiLabelDataset[String, String], featureIndex: Index[String], labelIndex: Index[String], initialZLabels: Array[Array[String]]): Array[LinearClassifier[String, String]] = {
    val localClassifiers: Array[LinearClassifier[String, String]] = new Array[LinearClassifier[String, String]](numberOfFolds)
    for (fold <- 0 until numberOfFolds) {
      Log.severe("Constructing dataset for the local model in fold #" + fold + "...")

      val (trainDataArray, testDataArray) = splitForFold(data.getDataArray, fold)
      val (trainPosLabels, testPosLabels) = splitForFold(data.getPositiveLabelsArray, fold)
      val (trainInitialZLabels, testInitialZLabels) = splitForFold(initialZLabels, fold)
      assert((trainDataArray.length == trainPosLabels.length))
      assert((trainInitialZLabels.length == trainPosLabels.length))
      assert((testDataArray.length == testPosLabels.length))
      assert((testInitialZLabels.length == testPosLabels.length))

      val dataset: GeneralDataset[String, String] = GroupRegression.makeLocalData(trainDataArray, labelIndex, featureIndex, fold, useRVF, trainInitialZLabels)
      Log.severe("Fold #" + fold + ": Training local model...")
      val factory: LinearClassifierFactory[String, String] = new LinearClassifierFactory[String, String](1e-4, false, zSigma)
      val localClassifier: LinearClassifier[String, String] = factory.trainClassifier(dataset)
      Log.severe("Fold #" + fold + ": Training of the local classifier completed.")
      val nilIndex: Int = labelIndex.indexOf(JointlyTrainedRelationExtractor.UNRELATED)
      Log.severe("Fold #" + fold + ": Evaluating the local classifier on the hierarchical dataset...")
      var total: Int = 0
      var predicted: Int = 0
      var correct: Int = 0
      for (i <- 0 until testDataArray.length) {
        val group = testDataArray(i)
        val gold: java.util.Set[Integer] = testPosLabels(i)
        val pred: java.util.Set[Integer] = new java.util.HashSet[Integer]
        for (j <- 0 until group.length) {
          val datum: Datum[String, String] = group(j)
          val scores: Counter[String] = localClassifier.scoresOf(datum)
          val sortedScores: List[(String, Double)] = GroupRegression.sortPredictions(scores)
          val sys: Int = labelIndex.indexOf(sortedScores.head._1)
          if (sys != nilIndex) pred.add(sys)
        }
        total += gold.size
        predicted += pred.size
        import scala.collection.JavaConversions._
        for (pv <- pred) {
          if (gold.contains(pv)) correct += 1;
        }
      }

      val p: Double = correct.toDouble / predicted.toDouble
      val r: Double = correct.toDouble / total.toDouble
      val f1: Double = (if (p != 0 && r != 0) 2 * p * r / (p + r) else 0)
      Log.severe("Fold #" + fold + ": Training score on the hierarchical dataset: P " + p + " R " + r + " F1 " + f1)
      Log.severe("Fold #" + fold + ": Created the Z classifier with " + labelIndex.size + " labels and " + featureIndex.size + " features.")
      localClassifiers(fold) = localClassifier
    }
    return localClassifiers
  }

  protected def initializeZDataset(totalSentences: Int, zLabels: Array[Array[Int]], data: Array[Array[Datum[String, String]]]): GeneralDataset[String, String] = {
    var dataset: GeneralDataset[String, String] = null
    if (useRVF) {
      dataset = new RVFDataset[String, String](featureIndex, zLabelIndex)
    }
    else {
      dataset = new Dataset[String, String](featureIndex, zLabelIndex)
    }
    var count: Int = 0
    for (i <- 0 until data.length) {
      val group: Array[Datum[String, String]] = data(i)
      for (s <- 0 until group.length) {
        val datum: Datum[String, String] = group(s)
        if (useRVF) {
          (datum.asInstanceOf[RVFDatum[String, String]]).setLabel(zLabelIndex.get(zLabels(i)(s)))
        }
        else {
          (datum.asInstanceOf[BasicDatum[String, String]]).setLabel(zLabelIndex.get(zLabels(i)(s)))
        }
        dataset.add(datum)
        count += 1
      }
    }
    Log.severe("Created the Z dataset with " + count + " datums.")
    return dataset
  }

  def predictZLabels(group: Array[Datum[String, String]], zLabels: Array[Int], zClassifier: LinearClassifier[String, String]): Array[Int] =
    for {
      datum <- group
      probs = zClassifier.logProbabilityOf(datum)
    } yield zLabelIndex.indexOf(Counters.argmax(probs))

  private[twitter4food] def computeZLogProbs(group: Array[Datum[String, String]], zLogProbs: Array[Counter[String]], zClassifier: LinearClassifier[String, String], epoch: Int) {
    {
      var s: Int = 0
      while (s < group.length) {
        {
          zLogProbs(s) = zClassifier.logProbabilityOf(group(s))
        }
        ({
          s += 1;
          s - 1
        })
      }
    }
  }

  /** updates the zLabels array with new predicted z labels */
  private[twitter4food] def inferZLabelsStable(group: Array[Datum[String, String]], positiveLabels: Set[Integer], negativeLabels: Set[Integer], zLabels: Array[Int], zLogProbs: Array[Counter[String]], zClassifier: LinearClassifier[String, String], epoch: Int) {
    val showProbs: Boolean = false
    val verbose: Boolean = true
    assert((positiveLabels.size == 1))
    if (verbose) {
      System.err.print("inferZLabels: ")
      if (positiveLabels.size > 1) System.err.println("MULTI RELATION")
      else if (positiveLabels.size == 1) System.err.println("SINGLE RELATION")
      else System.err.println("NIL RELATION")
      System.err.println("positiveLabels: " + positiveLabels)
      System.err.println("negativeLabels: " + negativeLabels)
      System.err.print("Current zLabels:")
      zLabels.foreach(i => System.err.print(" " + i))
      System.err.println
    }
    computeZLogProbs(group, zLogProbs, zClassifier, epoch)
    for (s <- 0 until group.length) {
      var maxProb: Double = Double.NegativeInfinity
      var bestLabel: Int = -1
      val zProbabilities: Counter[String] = zLogProbs(s)
      val jointProbabilities: Counter[String] = new ClassicCounter[String]
      val origZLabel: Int = zLabels(s)
      import scala.collection.JavaConversions._
      for (candidate <- zProbabilities.keySet) {
        val candidateIndex: Int = zLabelIndex.indexOf(candidate)
        if (showProbs) System.err.println("\tProbabilities for z[" + s + "]:")
        var prob: Double = zProbabilities.getCount(candidate)
        zLabels(s) = candidateIndex
        if (showProbs) System.err.println("\t\tlocal (" + zLabels(s) + ") = " + prob)
        val yLabel: String = yLabelIndex.get(positiveLabels.iterator.next)
        val yDatum: Datum[String, String] = new RVFDatum[String, String](extractYFeatures(zLabels), "")
        val yProbabilities: Counter[String] = yClassifier.logProbabilityOf(yDatum)
        val v: Double = yProbabilities.getCount(yLabel)
        if (showProbs) System.err.println("\t\t\ty+ (" + yLabel + ") = " + v)
        prob += v
        if (showProbs) System.err.println("\t\ttotal (" + zLabels(s) + ") = " + prob)
        jointProbabilities.setCount(candidate, prob)
        if (prob > maxProb) {
          maxProb = prob
          bestLabel = zLabels(s)
        }
      }
      if (bestLabel != -1 && bestLabel != origZLabel) {
        if (verbose) System.err.println("\tNEW zLabels[" + s + "] = " + bestLabel)
        zLabels(s) = bestLabel
        zUpdatesInOneEpoch += 1
      }
      else {
        zLabels(s) = origZLabel
      }
    }
  }

  /**
   * Implements weighted voting over the different Z classifiers in each fold
   * @return Probabilities (NOT log probs!) for each known label
   */
  def classifyLocally(datum: Datum[String, String]): Counter[String] = localClassificationMode match {
    case WeightedVote => {
      val sumProbs: Counter[String] = new ClassicCounter[String]
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

  def classifyMentions(sentences: List[Datum[String, String]]): Counter[String] = {
    throw new UnsupportedOperationException("classifyMentions not implemented!")
  }
}