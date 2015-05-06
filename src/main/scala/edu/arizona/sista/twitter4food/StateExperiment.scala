package edu.arizona.sista.twitter4food

import edu.arizona.sista.learning._
import edu.arizona.sista.struct._
import Utils._
import java.io._
import java.util.Properties
import scala.collection.{immutable, mutable, GenSeq}
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util._
import StateExperiment._
import Mixins._
import de.bwaldvogel.liblinear.SolverType
import scala.io.Source
import collection.JavaConversions._
import edu.arizona.sista.utils.EvaluationStatistics

/**
 * Created by dfried on 1/14/14.
 */
class StateExperiment(parameters: ExperimentParameters, printWriter: PrintWriter = new java.io.PrintWriter(System.out))
  extends Experiment(parameters = parameters, printWriter = printWriter) {


  /**
   * Run LOOCV over the states with the given features, using the parameters
   * @param stateFeatures
   *                      A mapping from each state's name to its feature counter
   * @param actualLabels
   *                     A mapping from each state to its true label
   * @tparam L
   *           the label type
   * @return
   *         the results of the experiment: the predicted labels for each state, the actual labels (an exact copy of
   *         actualLabels), and the highly weighted features for predicting each label
   */
  def runSet[L](stateFeatures: Map[String, Counter[String]], actualLabels: Map[String, L]): ExperimentResults[L] = {
    val labels = actualLabels.values.toSet

    // keep track of the highly weighted features for each label. This will be updated by updateWeights in each fold
    // of the cross validation
    val weights = new mutable.HashMap[L, Counter[String]]()
    for (label <- labels) weights(label) = new Counter[String]

    def updateWeights(clf: Classifier[L, String]) = {
      val marginalWeights: Option[Map[L, Counter[String]]] = clf match {
        case clf:LiblinearClassifier[L, String] => Some(clf.getWeights(verbose = false))
        //case clf:BaggingClassifier[L, String] => clf.getWeights
        case _ => None
      }
      if (marginalWeights != None) {
        for ((l, w) <- marginalWeights.get) {
          println(w.sorted.take(10) mkString(" "))
          // normalize the weights and add them into the total
          weights(l) = weights(l) + w / Math.sqrt(w.dotProduct(w))
        }
      }
    }

    // predict the labels for each state using LOOCV
    val predictedLabels: Map[String, L] = (for {
    // iterate over the loocv sets. heldOutState will iterate over each state, and trainingFeatures will contain
    // the remaining states along with their associated features
      (heldOutState: String, trainingFeatures: Map[String, Counter[String]])  <- loocvSets(1234)(stateFeatures)

      (clf, procFeats, featureSelector) = trainFromFeatures(trainingFeatures, actualLabels)

      // get returns an option, so use the arrow notation to assign the label inside the option to stateLabel
      stateLabel <- actualLabels.get(heldOutState)

      // make a datum for the state we're testing on (stateLabel will not be used!)
      testingDatum = mkDatum(stateLabel, procFeats(stateFeatures(heldOutState)))
      _ = { // this funky notation just allows us to do side effects in the for comprehension,
        // specifically updating the feature weights
        println(clf.toString)
        if (featureSelector != None)
          println(featureSelector.get.featureScores.toSeq.sortBy(_._2).reverse.take(20))
        if (parameters.classifierType == SVM_L1 || parameters.classifierType == SVM_L2) {
          // get the feature weights for this state for the true class
          updateWeights(clf)
        }
      }
      prediction = clf.classOf(testingDatum)
    } yield heldOutState -> prediction).toMap

    // return the predicted labels and the mean feature weights
    ExperimentResults(predictedLabels, actualLabels, weights.toMap)
  }

  /**
   * run the experiment on the given sequence of tweets, creating the datasets, aggregating tweets,
    and doing the training and classification
   * @param tweets
   * @return a map from the dataset name (e.g. "overweight", "diabetes") to the classification results for that
   *         experiment
   */
  def run(tweets: Seq[Tweet]): Map[String, ExperimentResults[Int]] = {
    val geotagger = new GeoTagger

    val diabetes = Datasets.diabetes
    val diabetesLabels: Map[String, Int] = bin(parameters.numClasses)(normLocationsInMap(diabetes, geotagger),
      parameters.removeMarginals)

    val overweight = Datasets.overweight
    val overweightLabels: Map[String, Int] = bin(parameters.numClasses)(normLocationsInMap(overweight, geotagger),
      parameters.removeMarginals)

    // val illiteracy = Datasets.illiteracy
    // val illiteracyLabels: Map[String, Int] = bin(parameters.numClasses)(normLocationsInMap(illiteracy, geotagger), parameters.removeMarginals)

    val political = Datasets.political
    val politicalLabels: Map[String, Int] = bin(parameters.numClasses)(political, parameters.removeMarginals)

    val sets = Map(
      "overweight" ->  overweightLabels,
      "diabetes" ->  diabetesLabels,
      "political" ->  politicalLabels
      //"illiteracy" -> illiteracyLabels
    )

    // val regionsCoarseBaseline = sets mapValues predictMajorityGrouped(Datasets.regionsCoarse)
    // val regionsFineBaseline = sets mapValues predictMajorityGrouped(Datasets.regionsFine)
    // val regionsCoarseAccuracy = regionsCoarseBaseline.zipWith(accuracy)(sets)
    // val regionsFineAccuracy = regionsFineBaseline.zipWith(accuracy)(sets)

    printWriter.println(s"numClasses: ${parameters.numClasses}")
    printWriter.println(s"removeMarginals: ${parameters.removeMarginals}")

    /*
    printWriter.println
    printWriter.println("regions coarse baseline")
    printWriter.println("overall accuracy:")
    printWriter.println(regionsCoarseAccuracy.values.sum / regionsCoarseAccuracy.size)
    printWriter.println("per dataset accuracy:")
    printWriter.println(regionsCoarseAccuracy)
    printWriter.println("per region accuracy:")
    printWriter.println(sets mapValues(predictMajorityWithinGroupAccuracy(Datasets.regionsCoarse)))

    printWriter.println
    printWriter.println("regions fine baseline")
    printWriter.println("overall accuracy:")
    printWriter.println(regionsFineAccuracy.values.sum / regionsFineAccuracy.size)
    printWriter.println("per dataset accuracy:")
    printWriter.println(regionsFineAccuracy)
    printWriter.println("per region accuracy:")
    printWriter.println(sets mapValues(predictMajorityWithinGroupAccuracy(Datasets.regionsFine)))
    */

    val tweetsByState = tweets.groupBy( { tweet => tweet.normalizedLocation }).mapValues(_.toSeq).toSeq

    val (stateLabels: Seq[String], stateTweets: Seq[Seq[Tweet]]) = tweetsByState.unzip

    val viewFeatures = stateTweets.map(mkViewFeatures(parameters.lexicalParameters.ngramThreshold))
    // val lexicalFeatures = stateTweets.map(tweets => mkLexicalFeatures(tweets,  parameters.lexicalParameters.ngramThreshold))
    // val ldaFeatures = stateTweets.map(mkLdaFeatures)
    val regionalFeatures = stateTweets.map(mkRegionalFeatures)

    val featureSets = new collection.mutable.ArrayBuffer[Seq[Counter[String]]]

    /*
     parameters.reduceLexicalK match {
      case Some(k) => featureSets.append(NMF.reduceDimensionalityC(lexicalFeatures, k, "rLEX_" + _))
      case None => featureSets.append(lexicalFeatures)
    }

    parameters.reduceLdaK match {
      case Some(k) => featureSets.append(NMF.reduceDimensionalityC(ldaFeatures, k, "rLDA_" + _))
      case None => featureSets.append(ldaFeatures)
    }
    */

    featureSets.append(viewFeatures)
    featureSets.append(regionalFeatures)

    val stateFeatures: Map[String, Counter[String]] = (for {
      (stateLabel, index) <- stateLabels.zipWithIndex
      featuresForState = featureSets.map(_(index)).reduce(_ + _)
      if (stateLabel) != null
    } yield stateLabel -> featuresForState).toMap

    // make features by aggregating all tweets for a state
    val predictionsAndWeights = (for {
      (name, labels) <- sets.toList
    } yield (name -> runSet(stateFeatures, labels))).toMap

    predictionsAndWeights
  }
}

object StateExperiment {

  def geotaggedTweets(tweets: Iterable[Tweet]): Iterable[Tweet] =
    tweets filter { tweet => tweet.normalizedLocation != None }

  def normLocationsInMap(map: Map[String, Float], geotagger: GeoTagger): Map[String, Float] = for {
    (state, value) <- map
    // will ignore any states that can't be normalized to state abbreviations by the geotagger
    abbreviation <- geotagger.normalizeLocation(state, null)
  } yield (abbreviation, value)

  def mean(map: Map[_, Float]): Float = {
    map.values.sum / map.values.size
  }

  def insertionLocation(x: Float, ys: Seq[Float]): Int = {
    for ((y, i) <- ys.zipWithIndex) if (y >= x) return i
    return ys.length
  }

  def threshold[A](map: Map[A,Float], splitPoints: Seq[Float], removeMarginals: Option[Int] = None): Map[A,  Int] = {
    // remove marginals: if an int k, drops the k closest datapoints to each boundary, on each side of the boundary
    val insertPoints =  for {
      (key, value) <- map
    } yield (key, insertionLocation(value, splitPoints))
    removeMarginals match {
      case None => insertPoints
      case Some(toDrop) => {
        val grouped: Map[Int, Map[A, Float]] = map.groupBy(pair => insertPoints(pair._1))
        val toKeep = (for {
          (k, vals) <- grouped
          sorted = vals.toSeq.sortBy(_._2).map(_._1)
          maybeDropHead = (if (k != 0) sorted.drop(toDrop) else sorted).reverse
          maybeDropTail = (if (k != splitPoints.size) maybeDropHead.drop(toDrop) else maybeDropHead)
        } yield maybeDropTail).flatten.toSet
        insertPoints.filterKeys(toKeep.contains(_))
      }
    }
  }

  def zipMaps[K, V1, V2](m1: Map[K, V1], m2: Map[K, V2]): Map[K, (V1, V2)] = (for {
    key <- m1.keySet.intersect(m2.keySet)
  } yield key -> (m1(key), m2(key))).toMap

  def accuracy[K, V](predicted: Map[K, V], actual: Map[K, V]): Float = {
    val zipped = zipMaps(predicted, actual)
    val matchCount = zipped.values.count({case ((v1, v2)) => v1 == v2})
    assert(zipped.size == predicted.size)
    assert(zipped.size == actual.size)
    matchCount.toFloat / zipped.size
  }

  def quantiles(q: Int)(values: Seq[Float]): Seq[Float] = {
    // R-2, SAS-5, http://en.wikipedia.org/wiki/Quantile#Estimating_the_quantiles_of_a_population
    val sortedValues = values.sorted
    val N = values.length
    for {
      p <- 1 until q
      h = p.toFloat * N / q + 0.5
      lower = sortedValues(Math.ceil(h - 0.5).toInt - 1)
      upper = sortedValues(Math.floor(h + 0.5).toInt - 1)
    } yield (lower + upper) / 2
  }

  def bin[K](numberOfBins: Int)(m: Map[K, Float], removeMarginals: Option[Int] = None) = {
    val splits = quantiles(numberOfBins)(m.values.toSeq)
    threshold(m, splits, removeMarginals)
  }

  def binVals(numberOfBins: Int)(m: Map[String, Counter[String]]): Map[String, Counter[String]] = {
    numberOfBins match {
      case noBinning if numberOfBins == 0 => m
      case error if numberOfBins < 0 => m
      case _ =>
        val splits = (for {
          feat <- (for (c <- m.values) yield c.keySet).toSeq.flatten
        } yield (feat, quantiles(numberOfBins)((for (c <- m.values) yield c.getCount(feat).toFloat).toSeq))).toMap

        val binned = (for {
          (key, count) <- m
          c = new Counter[String]
          dummy = for (feat <- count.keySet) c.setCount(feat, insertionLocation(count.getCount(feat).toFloat, splits(feat)))

        } yield key -> c).toMap

        binned
    }
  }

  def loocvSets[K, L, F](featureMap: Map[K, F]): Map[K, Map[K, F]] = {
    for {
      (heldOutKey, _) <- featureMap
      trainingFeatures = featureMap.filterKeys(_ != heldOutKey)
    } yield heldOutKey -> trainingFeatures
  }

  def predictMajority[K, L](m: Map[K, L]): Map[K, L] = for {
      (heldout, trainingFold) <- loocvSets(m)
      majorityClass = (new Counter(trainingFold.values)).toSeq.maxBy(_._2)._1
    } yield (heldout -> majorityClass)

  def predictMajorityNoCV[K, L](m: Map[K, L]): Map[K, L] = {
    val majorityClass = (new Counter(m.values)).toSeq.maxBy(_._2)._1
    m.mapValues(_ => majorityClass)
  }

  def predictMajorityGrouped[K, G, L](groups: Map[K, G])(m: Map[K, L]): Map[K, L] =
  // m is a dictionary that maps keys to binary classifications. This function takes a mapping of the same keys to
  // groups, and then returns a new map where each key is mapped to the majority class within its own group
    m.groupBy({ case (k, v) => groups(k) }).mapValues(map => predictMajority(map)).values.reduce(_ ++ _)

  def predictMajorityWithinGroupAccuracy[K, G, L](groups: Map[K, G])(m: Map[K, L]): Map[G, Float] =  {
    val predictions = predictMajorityGrouped(groups)(m)
    m.groupBy({ case (k, v) => groups(k) }).zipWith(accuracy)(predictions.groupBy({case (k, v) => groups(k) }))
  }

  def printWeights(p: java.io.PrintWriter)(params: ExperimentParameters, counterMap: Map[String, Map[Int, Counter[String]]]) : Unit = {
    if (params.classifierType == SVM_L2 || params.classifierType == SVM_L1)  {
      p.println(params)

      for ((name, weightsByLabel) <- counterMap) {
        p.println(name.toUpperCase)
        for ((label, weights) <- weightsByLabel) {
          p.println(label)
          def rep(list: Seq[(String, Double)]): Seq[String] =
            list.map({case (feature, weight) => "%s %.4f".format(feature, weight)})

          val sortedWeights = weights.sorted
          p.println("+\t-")
          p.println(rep(sortedWeights.take(20)).zip(rep(sortedWeights.reverse.take(20))).map({ case (s1, s2) => "%s\t%s".format(s1, s2) }).mkString("\n"))
          p.println
        }
        p.println
      }
      p.println
    }
  }

  def main(args: Array[String]) = {
    println(s"heap size: ${Runtime.getRuntime.maxMemory / (1024 * 1024)}")

    val outFile = if (args.size > 0) args(0) else null
    val tweets: Seq[Tweet] = (new SolrPager()).tweetsWithLocation

    val pw: PrintWriter = if (outFile != null) (new PrintWriter(new java.io.File(outFile))) else (new PrintWriter(System.out))

    // create many possible variants of the experiment parameters, and for each map to results of running the
    // experiment
    // notation: = assigns a parameter to a single value
    //           <- means the parameter will take on all of the values in the list in turn
    val predictionsAndWeights = (for {
        // which base tokens to use? e.g. food words, hashtags, all words
        tokenTypes: TokenType <- List(AllTokens, HashtagTokens, FoodTokens, FoodHashtagTokens).par
        // which annotators to use in addition to tokens?
        annotators <- List(
          //List(LDAAnnotator(tokenTypes), SentimentAnnotator),
          //List(SentimentAnnotator),
          List(LDAAnnotator(tokenTypes)),
          List())
        // type of normalization to perform: normalize across a feature, across a state, or not at all
        // this has been supplanted by our normalization by the number of tweets for each state
        normalization = NoNorm
        // only keep ngrams occurring this many times or more
        ngramThreshold = Some(2)
        // split feature values into this number of quantiles
        numFeatureBins = Some(3)
        // use a bias in the SVM?
        useBias = false
        // use regions as features?
        regionType = NoRegions
        classifierType = RandomForest

        // Some(k) to use k classifiers bagged, or None to not do bagging
        baggingNClassifiers <- List(None)
        // force use of features that we think will be informative in random forests?
        forceFeatures = true
        // how many classes should we bin the numerical data into for classification?
        numClasses = 2
        // Some(k) to keep k features ranked by mutual information, or None to not do this
        miNumToKeep: Option[Int] = None
        // Some(k) to limit random forest tree depth to k levels, or None to not do this
        maxTreeDepth: Option[Int] = Some(3)
        // these were from failed experiments to use NNMF to reduce the feature space
        //reduceLexicalK: Option[Int] = None
        //reduceLdaK: Option[Int] = None
        // Some(k) to remove the k states closest to the bin edges when binning numerical data into classification,
        // or None to use all states
        removeMarginals: Option[Int] = None

        params = new ExperimentParameters(new LexicalParameters(tokenTypes, annotators, normalization, ngramThreshold, numFeatureBins),
          classifierType, useBias, regionType, baggingNClassifiers, forceFeatures, numClasses,
          miNumToKeep, maxTreeDepth, removeMarginals)
      // Try is an object that contains either the results of the method inside or an error if it failed
      } yield params -> Try(new StateExperiment(params, pw).run(tweets))).seq

    // print the results and statistical significances for each parameter set
    for ((tokenType, group) <- predictionsAndWeights.groupBy({ case (params,  _) => params.lexicalParameters.tokenTypes })) {
      pw.println(s"tokenType: ${tokenType}")
      val (baselineParams, Success(baselineModel)) = group.filter({ case (params,  _) => params.lexicalParameters.annotators.isEmpty }).head
      for ((params, Success(treatment)) <- group) {
        pw.println("\tannotators: " + params.lexicalParameters.annotators.map(_.toString).mkString("+"))

        val isBaseline = (params == baselineParams)

        // aggregate over the various datasets
        var intraPredicted = new ArrayBuffer[Int]
        var intraActual = new ArrayBuffer[Int]
        var intraBaseline = new ArrayBuffer[Int]
        var intrAccuracies = new ArrayBuffer[Double]
        for ((datasetName, ExperimentResults(predicted, actual, _)) <- treatment) {
          val baseline = if (isBaseline) predictMajorityNoCV(actual) else baselineModel(datasetName).predictedLabels
          val states = baseline.keySet.intersect(predicted.keySet).intersect(actual.keySet).toSeq
          val es = new EvaluationStatistics[Int](states map(predicted), states map(actual))
          val accuracy = es.accuracy * 100
          intrAccuracies.append(accuracy)
          intraPredicted ++= states map(predicted)
          intraActual ++= states map(actual)
          intraBaseline ++= states map(baseline)
          val pvalue = EvaluationStatistics.classificationAccuracySignificance(states map(predicted), states map(baseline), states map(actual))
          pw.println(f"\t\t$datasetName%s")
          pw.println(f"\t\t$accuracy%2.2f (p = $pvalue%2.2f)")
        }
        val aggAccuracy = intrAccuracies.sum / intrAccuracies.length
        val aggPvalue = EvaluationStatistics.classificationAccuracySignificance(intraPredicted, intraBaseline,  intraActual)
        pw.println("\t\taverage")
        pw.println(f"\t\t$aggAccuracy%2.2f (p = $aggPvalue%2.2f)")
      }
    }

    pw.println
    pw.println("feature weights")

    for ((params, Success(resultsByDataset)) <- predictionsAndWeights.sortBy(_._1.toString)) {
      printWeights(pw)(params, resultsByDataset.mapValues(_.featureWeightsPerClass))
    }

    if (outFile != null) {
      try {
      } finally { pw.close() }
    } else {
      pw.flush()
    }
  }
}
