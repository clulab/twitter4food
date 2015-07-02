package edu.arizona.sista.twitter4food

import edu.arizona.sista.struct._
import java.io._
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import edu.arizona.sista.utils.EvaluationStatistics
import edu.arizona.sista.utils.StringUtils

/**
 * Created by dfried on 5/6/15.
 */
class IndividualsMIML(parameters: ExperimentParameters, printWriter: PrintWriter = new java.io.PrintWriter(System.out), val zSigma: Double = 1.0, val ySigma: Double = 1.0)
  extends Experiment(parameters = parameters, printWriter = printWriter) {

  def run(trainingCorpus: Seq[IndividualsTweets], testingCorpus: Seq[IndividualsTweets], stateLabels: Map[String, String], onlyFoodTweets: Boolean = false, realValued: Boolean = true) = {

    val trainingTweets = trainingCorpus.map(it => if (onlyFoodTweets) filterFoodTweets(it.tweets) else it.tweets)
    val testingTweets = testingCorpus.map(it => if (onlyFoodTweets) filterFoodTweets(it.tweets) else it.tweets)

    val (trainingFeatures, filterFn) =  mkViewFeatures(parameters.lexicalParameters.ngramThreshold)(trainingTweets)
    val testingFeatures: Seq[Counter[String]] = mkViewFeatures(None)(testingTweets)._1.map(_.filter(p => filterFn(p._1)))

    //val (processedFeatures, processFeaturesFn, _) = processFeatures(trainingFeatures, Seq()) // don't need to pass labels since we're not doing MI selection

    //def binarize(counter: Counter[String]) = new Counter(counter.keySet)

    //val (processedFeatures, processFeaturesFn) = (trainingFeatures.map(binarize), binarize _)

    val rescaledTrainingFeatures = trainingFeatures
    val rescaledTestingFeatures = testingFeatures

    val stateMIMLs = for {
      (Some(state), group) <- (rescaledTrainingFeatures zip trainingCorpus).groupBy((_._2.state)).toSeq
      stateFeatures: Seq[Counter[String]] = group.map(_._1)
      label = stateLabels(state)
    } yield MIML[String, String](stateFeatures, Set(label))


    val miml = new MIMLWrapper("/tmp/test.dat", realValued = realValued, zSigma = zSigma, ySigma = ySigma)
    miml.train(stateMIMLs)

    val predictedLabels = for {
      features <- rescaledTestingFeatures
      predictions = miml.classifyIndividual(features)
    } yield predictions.head._1

    printWriter.println(miml.jbre.zClassifiers(0).toBiggestWeightFeaturesString(true, 20, true))

    predictedLabels

  }

}

object IndividualsMIML {
  import IndividualsBaseline.labelledAccuracy

  def main(args: Array[String]) {
    import Experiment._

    val props = StringUtils.argsToProperties(args, verbose=false)

    val realValued = StringUtils.getBool(props, "realValued", true)

    val zSigma = StringUtils.getDouble(props, "zSigma", 1.0)
    val ySigma = StringUtils.getDouble(props, "ySigma", 1.0)

    val evaluateOnDev = StringUtils.getBoolOption(props, "evaluateOnDev").get

    // Some(k) to remove the k states closest to the bin edges when binning numerical data into classification,
    // or None to use all states
    val removeMarginals: Option[Int] = None
    // how many classes should we bin the numerical data into for classification?
    val numClasses = 2

    println(s"heap size: ${Runtime.getRuntime.maxMemory / (1024 * 1024)}")

    val outFile = if (args.size > 0) args(0) else null

    val individualsCorpus = new IndividualsCorpus("/data/nlp/corpora/twitter4food/foodSamples-20150501", "/data/nlp/corpora/twitter4food/foodSamples-20150501/annotations.csv", numToTake=Some(500))

    val stateLabels = Experiment.makeLabels(Datasets.overweight, numClasses, removeMarginals).mapValues(_.toString)

    val trainingTweets = IndividualsBaseline.makeBaselineTraining(numClasses, removeMarginals)(individualsCorpus)
    val testingTweets = if (evaluateOnDev) individualsCorpus.devTweets else individualsCorpus.testingTweets

    val pw: PrintWriter = if (outFile != null) (new PrintWriter(new java.io.File(outFile))) else (new PrintWriter(System.out))

    // create many possible variants of the experiment parameters, and for each map to results of running the
    // experiment
    // notation: = assigns a parameter to a single value
    //           <- means the parameter will take on all of the values in the list in turn
    val predictionsAndWeights = (for {
    // which base tokens to use? e.g. food words, hashtags, all words
      // tokenTypes: TokenType <- List(AllTokens, HashtagTokens, FoodTokens, FoodHashtagTokens).par
      tokenTypes: TokenType <- List(AllTokens).par
      // which annotators to use in addition to tokens?
      annotators <- List(
        //List(LDAAnnotator(tokenTypes), SentimentAnnotator),
        //List(SentimentAnnotator),
        // List(LDAAnnotator(tokenTypes)),
        List())
      // classifierType <- List(RandomForest, SVM_L2)
      classifierType: ClassifierType <- List(SVM_L2)
      // type of normalization to perform: normalize across a feature, across a state, or not at all
      // this has been supplanted by our normalization by the number of tweets for each state
      normalization = NoNorm
      // only keep ngrams occurring this many times or more
      ngramThreshold = Some(3)
      // split feature values into this number of quantiles
      numFeatureBins = classifierType match {
        case RandomForest => Some(3)
        case _ => None
      }
      // use a bias in the SVM?
      useBias = false
      // use regions as features?
      regionType = NoRegions

      // Some(k) to use k classifiers bagged, or None to not do bagging
      baggingNClassifiers <- List(None)
      // force use of features that we think will be informative in random forests?
      forceFeatures = classifierType match {
        case RandomForest => true
        case _ => false
      }
      // Some(k) to keep k features ranked by mutual information, or None to not do this
      miNumToKeep: Option[Int] = None
      // Some(k) to limit random forest tree depth to k levels, or None to not do this
      maxTreeDepth: Option[Int] = Some(3)
      // these were from failed experiments to use NNMF to reduce the feature space
      //reduceLexicalK: Option[Int] = None
      //reduceLdaK: Option[Int] = None

      filterFoodTweets = true

      params = new ExperimentParameters(new LexicalParameters(tokenTypes, annotators, normalization, ngramThreshold, numFeatureBins),
        classifierType, useBias, regionType, baggingNClassifiers, forceFeatures, numClasses,
        miNumToKeep, maxTreeDepth, removeMarginals, featureScalingFactor = Some(1000.0))
    } yield params -> new IndividualsMIML(params, pw, zSigma = zSigma, ySigma = ySigma).run(trainingTweets, testingTweets, stateLabels, filterFoodTweets, realValued = realValued)).seq

    for ((params, predictions) <- predictionsAndWeights.sortBy(_._1.toString)) {
      pw.println(params)

      val actual = testingTweets.map(_.label.get)
      val intPredictions = predictions.map(_.toInt)

      // baseline
      val majorityBaseline: Seq[Int] = predictMajorityNoCV(actual)
      val labelledBaseline = testingTweets zip majorityBaseline

      val (baselineCorrect, baselineTotal) = labelledAccuracy(labelledBaseline)
      pw.println(s"overall accuracy\t${baselineCorrect} / ${baselineTotal}\t${baselineCorrect.toDouble / baselineTotal * 100.0}%")
      pw.println

      // system
      val labelledInstances: Seq[(IndividualsTweets, Int)] = testingTweets zip intPredictions
      val (correct, total) = labelledAccuracy(labelledInstances)
      val pvalue = EvaluationStatistics.classificationAccuracySignificance(intPredictions, majorityBaseline, actual)
      pw.println(s"overall accuracy\t${correct} / ${total}\t${correct.toDouble / total * 100.0}%\t(p = ${pvalue})")
      pw.println

      val byClass: Map[Int, Seq[(IndividualsTweets, Int)]] = labelledInstances.groupBy(_._1.label.get)

      val byClassAccuracy = byClass.mapValues(labelledAccuracy).toMap
      // print the per-class accuracy
      pw.println("accuracy by class")
      for ((class_, (correct, total)) <- byClassAccuracy) {
        pw.println(s"class ${class_}\t${correct} / ${total}\t${correct.toDouble / total * 100.0}%")
      }
      pw.println

      /*
      if (predictCelebrities) {
        // print each prediction
        for ((tweets, prediction) <- labelledInstances.sortBy( { case (it, prediction) => (it.label.get, it.username) } )) {
          pw.println(s"${tweets.username}\tact: ${tweets.label.get}\tpred: ${prediction}")
        }
      } else*/ {
        // print predictions by state
        for ((state, statesInstances) <- labelledInstances.groupBy( { case (it, prediction)  => it.state.get }).toSeq.sortBy(_._1)) {
          val (correct, total) = labelledAccuracy(statesInstances)
          pw.println(s"${state}\t${correct} / ${total}\t${correct.toDouble / total * 100.0}%")
        }
      }

      pw.println
      pw.println
    }

    pw.println
    pw.println

    if (outFile != null) {
      try {
      } finally { pw.close() }
    } else {
      pw.flush()
    }
  }

}
