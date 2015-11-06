package edu.arizona.sista.twitter4food

import java.io._

import edu.arizona.sista.struct._
import edu.arizona.sista.utils.{EvaluationStatistics, StringUtils}

import scala.collection.mutable.ArrayBuffer

/**
 * Created by dfried on 4 Nov 2015
 */
class IndividualsRegression(parameters: ExperimentParameters,
                            printWriter: PrintWriter = new java.io.PrintWriter(System.out),
                            logger: Option[PrintWriter] = None,
                            val onlyLocalTraining: Boolean = false,
                            val zSigma: Double = 1.0,
                            val positiveClass: String = "1",
                            val negativeClass: String = "0")
  extends Experiment(parameters = parameters, printWriter = printWriter) {

  def run(trainingCorpus: Seq[IndividualsTweets],
          testingCorpus: Seq[IndividualsTweets],
          stateLabels: Map[String, String],
          stateValues: Map[String, Double],
          onlyFoodTweets: Boolean = false,
          realValued: Boolean = true,
          featureSerializationPath: Option[String] = None,
          randomSeed: Int = 1234) = {

    val random = new util.Random(randomSeed)

    val trainingTweets = trainingCorpus.map(it => if (onlyFoodTweets) filterFoodTweets(it.tweets) else it.tweets)
    val testingTweets = testingCorpus.map(it => if (onlyFoodTweets) filterFoodTweets(it.tweets) else it.tweets)

    val (trainingFeatures, filterFn) =  mkViewFeatures(parameters.lexicalParameters.ngramThreshold)(trainingTweets)
    val testingFeatures: Seq[Counter[String]] = mkViewFeatures(None)(testingTweets)._1.map(_.filter(p => filterFn(p._1)))

    def labelIndividuals(individuals: Seq[IndividualsTweets], targetValue: Double): Array[String] = {
      Array.tabulate[String](individuals.size)(i => if (random.nextDouble() < targetValue) positiveClass else negativeClass)
    }

    val stateNames = new ArrayBuffer[String]()
    val (individualDataByState, individualLabelsByState, valuesByState) = (for {
      (Some(state), featuresAndInds) <- (trainingFeatures, trainingCorpus).zipped.groupBy((_._2.state)).toArray
      (individualFeatures, individuals) = featuresAndInds.toArray.unzip
      individualData = individualFeatures.map(MIMLWrapper.counterToRVFDatum[String,String])
      value = stateValues(state)
      individualLabels = labelIndividuals(individuals, value)
      _ = stateNames.append(state)
    } yield (individualData, individualLabels, value)).unzip3

    val regressor = new MultipleInstancesRegression[String,String](positiveClass=positiveClass, negativeClass=negativeClass, zSigma=zSigma, onlyLocalTraining=onlyLocalTraining, logger=logger)
    regressor.train(individualDataByState, individualLabelsByState, valuesByState, Some(stateNames.toArray))

    val predictedLabels = for {
      features <- testingFeatures
      predictions = regressor.classifyLocally(MIMLWrapper.counterToRVFDatum(features))
    } yield MultipleInstancesRegression.sortPredictions(predictions).maxBy(_._2)._1

    printWriter.println(regressor.zClassifiers(0).toBiggestWeightFeaturesString(true, 20, true))

    predictedLabels

  }

}

object IndividualsRegression {
  import IndividualsBaseline.labelledAccuracy

  def main(args: Array[String]) {
    import Experiment._

    val props = StringUtils.argsToProperties(args, verbose=true)

    val onlyLocalTraining = StringUtils.getBool(props, "onlyLocalTraining", false)

    val zSigma = StringUtils.getDouble(props, "zSigma", 1.0)

    val evaluateOnDev = StringUtils.getBoolOption(props, "evaluateOnDev").get

    val excludeUsersWithMoreThan = StringUtils.getIntOption(props, "excludeUsersWithMoreThan")

    val resultsOut = StringUtils.getStringOption(props, "resultsOut")

    val organizationsFile = StringUtils.getStringOption(props, "organizationsFiles")

    val maxUsersPerState = StringUtils.getIntOption(props, "maxUsersPerState")

    val corpusLocation = props.getProperty("corpusLocation", "/data/nlp/corpora/twitter4food/foodSamples-20150501")
    val annotationsLocation = props.getProperty("annotationsLocation", "/data/nlp/corpora/twitter4food/foodSamples-20150501/annotations.csv")

    val outFile = StringUtils.getStringOption(props, "outputFile")
    val pw: PrintWriter = outFile match {
      case Some(fileName) => new PrintWriter(new java.io.File(fileName))
      case None => new PrintWriter(System.out)
    }

    val logger: Option[PrintWriter] = StringUtils.getStringOption(props, "logFile").map(filename => new PrintWriter(new java.io.File(filename)))

    // Some(k) to remove the k states closest to the bin edges when binning numerical data into classification,
    // or None to use all states
    val removeMarginals: Option[Int] = None
    // how many classes should we bin the numerical data into for classification?
    val numClasses = 2

    println(s"heap size: ${Runtime.getRuntime.maxMemory / (1024 * 1024)}")

    val resultsPw: Option[PrintWriter] = resultsOut.map(filename => new PrintWriter(new java.io.File(filename)))

    val individualsCorpus = new IndividualsCorpus(corpusLocation, annotationsLocation, numToTake=maxUsersPerState, excludeUsersWithMoreThan=excludeUsersWithMoreThan, organizationsFile = organizationsFile)

    val dataset = Datasets.overweight
    val stateValues = normLocationsInMap(dataset, geotagger) // convert Arizona to AZ, for example
    val stateLabels = Experiment.makeLabels(dataset, numClasses, removeMarginals).mapValues(_.toString)

    val trainingTweets: Seq[IndividualsTweets] = IndividualsBaseline.makeBaselineTraining(numClasses, removeMarginals)(individualsCorpus)
    val testingTweets: List[IndividualsTweets] = if (evaluateOnDev) individualsCorpus.devTweets else individualsCorpus.testingTweets

    pw.println(s"${trainingTweets.size} training tweets")
    pw.println(s"${testingTweets.size} testing tweets")

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
      // type of normalization to perform: normalize across a feature, across a state, or not at all
      // this has been supplanted by our normalization by the number of tweets for each state
      normalization = NoNorm
      // only keep ngrams occurring this many times or more
      ngramThreshold = Some(3)
      // split feature values into this number of quantiles
      numFeatureBins = None
      // use a bias in the SVM?
      useBias = false
      // use regions as features?
      regionType = NoRegions

      // Some(k) to use k classifiers bagged, or None to not do bagging
      baggingNClassifiers <- List(None)
      // force use of features that we think will be informative in random forests?
      forceFeatures = false
      // Some(k) to keep k features ranked by mutual information, or None to not do this
      miNumToKeep: Option[Int] = None
      // Some(k) to limit random forest tree depth to k levels, or None to not do this
      maxTreeDepth: Option[Int] = Some(3)
      // these were from failed experiments to use NNMF to reduce the feature space
      //reduceLexicalK: Option[Int] = None
      //reduceLdaK: Option[Int] = None

      filterFoodTweets = true

      params = new ExperimentParameters(new LexicalParameters(tokenTypes, annotators, normalization, ngramThreshold, numFeatureBins),
        classifierType=SVM_L2, // note: this is ignored
        useBias, regionType, baggingNClassifiers, forceFeatures, numClasses,
        miNumToKeep, maxTreeDepth, removeMarginals, featureScalingFactor = Some(1.0))
    } yield params -> new IndividualsRegression(params, pw, logger = logger, onlyLocalTraining = onlyLocalTraining, zSigma = zSigma).run(trainingTweets, testingTweets, stateLabels, stateValues.mapValues(_.toDouble), filterFoodTweets)).seq

    for ((params, predictions) <- predictionsAndWeights.sortBy(_._1.toString)) {
      pw.println(params)

      val actual = testingTweets.map(_.label.get)
      val intPredictions = predictions.map(_.toInt)

      // baseline
      val majorityBaseline: Seq[Int] = predictMajorityNoCV(actual)
      val labelledBaseline = testingTweets zip majorityBaseline

      val (baselineCorrect, baselineTotal) = labelledAccuracy(labelledBaseline)
      pw.println(s"baseline overall accuracy\t${baselineCorrect} / ${baselineTotal}\t${baselineCorrect.toDouble / baselineTotal * 100.0}%")
      pw.println

      // system
      val labelledInstances: Seq[(IndividualsTweets, Int)] = testingTweets zip intPredictions
      val (correct, total) = labelledAccuracy(labelledInstances)
      val pvalue = EvaluationStatistics.classificationAccuracySignificance(intPredictions, majorityBaseline, actual)
      pw.println(s"system overall accuracy\t${correct} / ${total}\t${correct.toDouble / total * 100.0}%\t(p = ${pvalue})")
      pw.println

      val byClass: Map[Int, Seq[(IndividualsTweets, Int)]] = labelledInstances.groupBy(_._1.label.get)

      val byClassAccuracy = byClass.mapValues(labelledAccuracy).toMap
      // print the per-class accuracy
      pw.println("accuracy by class")
      for ((class_, (correct, total)) <- byClassAccuracy) {
        pw.println(s"class ${class_}\t${correct} / ${total}\t${correct.toDouble / total * 100.0}%")
      }
      pw.println

      def printPredictions(printWriter: PrintWriter): Unit = {
        // print each prediction
        printWriter.println("username,actual,predicted")
        for ((tweets, prediction) <- labelledInstances.sortBy( { case (it, prediction) => it.username } )) {
          printWriter.println(s"${tweets.username},${tweets.label.get},${prediction}")
        }

        pw.println
        pw.println
      }

      printPredictions(pw)
      resultsPw.foreach(printPredictions)

    }

    pw.println
    pw.println

    if (outFile != null) {
      try {
      } finally { pw.close() }
    } else {
      pw.flush()
    }

    resultsPw.foreach(_.close)

    logger.foreach(_.close)
  }
}


