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
class StateExperiment(parameters: ExperimentParameters, printWriter: PrintWriter = new java.io.PrintWriter(System.out), randomSeed: Int = 1234)
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
      (heldOutState: String, trainingFeatures: Map[String, Counter[String]])  <- loocvSets(randomSeed)(stateFeatures)
      (orderedFeats, orderedLabels) = (for {
        (state, features) <- trainingFeatures.toSeq
      } yield (features, actualLabels(state))).unzip
      (clf, procFeats, featureSelector) = trainFromFeatures(orderedFeats, orderedLabels)

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

    val diabetesLabels = Experiment.makeLabels(Datasets.diabetes, parameters.numClasses, parameters.removeMarginals)
    val overweightLabels = Experiment.makeLabels(Datasets.overweight, parameters.numClasses, parameters.removeMarginals)
    val politicalLabels = Experiment.makeLabels(Datasets.political, parameters.numClasses, parameters.removeMarginals)

    val sets = Map(
      "overweight" ->  overweightLabels,
      "diabetes" ->  diabetesLabels,
      "political" ->  politicalLabels
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
  import Experiment._

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
      printWeights(pw, params, resultsByDataset.mapValues(_.featureWeightsPerClass))
    }

    if (outFile != null) {
      try {
      } finally { pw.close() }
    } else {
      pw.flush()
    }
  }
}
