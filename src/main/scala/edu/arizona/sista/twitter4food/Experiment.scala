package edu.arizona.sista.twitter4food

import java.io.PrintWriter

import de.bwaldvogel.liblinear.SolverType
import edu.arizona.sista.learning._
import edu.arizona.sista.struct.Counter

trait ClassifierType

case object Linear_SVM extends ClassifierType
case object RBF_SVM extends ClassifierType
case object SVM_L2 extends ClassifierType
case object SVM_L1 extends ClassifierType
case object RandomForest extends ClassifierType

trait Regions
case object NoRegions extends Regions
case object CoarseRegions extends Regions
case object FineRegions extends Regions

case class LexicalParameters(tokenTypes: TokenType = AllTokens,
                             annotators: List[Annotator] = Nil,
                             normalization: NormalizationType = NoNorm,
                             ngramThreshold: Option[Int] = None,
                             numFeatureBins: Option[Int] = None) {
}

case class ExperimentParameters(lexicalParameters: LexicalParameters = LexicalParameters(),
                                classifierType: ClassifierType = SVM_L2,
                                useBias: Boolean = false,
                                regionType: Regions = NoRegions,
                                baggingNClassifiers: Option[Int] = None,
                                forceFeatures: Boolean = false,
                                numClasses: Int = 2,
                                miNumToKeep: Option[Int] = None,
                                maxTreeDepth: Option[Int] = None,
                                //reduceLexicalK: Option[Int] = None,
                                //reduceLdaK: Option[Int] = None,
                                removeMarginals: Option[Int] = None) {
  override def toString = s"""tokenTypes: ${lexicalParameters.tokenTypes}
annotators: ${lexicalParameters.annotators}
regionType: ${regionType}"""

}

case class ExperimentResults[L](predictedLabels: Map[String, L], actualLabels: Map[String, L], featureWeightsPerClass: Map[L,Counter[String]])

/**
 * Created by dfried on 5/6/15.
 */
class Experiment(val parameters: ExperimentParameters, val printWriter: PrintWriter = new java.io.PrintWriter(System.out)) {

  def processTweet(tweet: Tweet): Seq[String] = {
    TweetView.view(parameters.lexicalParameters.annotators)(parameters.lexicalParameters.tokenTypes)(tweet)
  }

  def mkViewFeatures(ngramThreshold: Option[Int])(tweets: Seq[Tweet]): Counter[String] = {
    val feats = new Counter[String](tweets.map(processTweet).flatten)
    val thresh = ngramThreshold match {
      case None => feats
      case Some(k) => feats.filterValues(_ >= k)
    }
    // scale each feature by the number of tweets aggregated (but divide by 1000 to avoid SVM numerical instability)
    thresh / (tweets.size / 1000.0)
  }

  // return a list of the names of features to force use of in the random forest
  def featuresToForce: Option[Set[String]] =
    if (parameters.forceFeatures)
      parameters.regionType match {
        case NoRegions => None
        case CoarseRegions => Some(Datasets.regionsCoarse.values.toSet.map(mkRegionalFeature))
        case FineRegions => Some(Datasets.regionsFine.values.toSet.map(mkRegionalFeature))
      } else None

  def mkRegionalFeature(regionName: String): String = "R_" + regionName

  def mkRegionalFeatures(tweets: Seq[Tweet]): Counter[String] = parameters.regionType match {
    case NoRegions => new Counter[String]
    // assume all are from the same state
    case CoarseRegions => new Counter[String](List(mkRegionalFeature(Datasets.regionsCoarse(tweets(0).normalizedLocation))))
    case FineRegions => new Counter[String](List(mkRegionalFeature(Datasets.regionsFine(tweets(0).normalizedLocation))))
  }

  def mkDataset[K, L](labelFn: (K => Option[L]), featureMap: Map[K, Counter[String]]): RVFDataset[L, String] = {
    val dataset = new RVFDataset[L, String]
    for ((key, features) <- featureMap)
      labelFn(key) match {
        case Some(label) => {
          dataset += new RVFDatum(label, features)
        }
        case _ => ()
      }
    dataset
  }

  def mkDatum[L](label: L, features: Counter[String]) = {
    new RVFDatum[L, String](label, features)
  }

  def loocvSets[K, L, F](seed: Int)(featureMap: Map[K, F]) = {
    val random = new scala.util.Random(seed)
    for {
      (heldOutKey, _) <- featureMap
      otherFeatures = featureMap.filterKeys(_ != heldOutKey)
      shuffled: List[(K, F)] = random.shuffle(otherFeatures.toList)
    } yield heldOutKey -> shuffled.toMap
  }

  def loocvTrainAndDevSets[K, L, F](seed: Int, numInTrain: Int)(featureMap: Map[K, F]) = {
    val random = new scala.util.Random(seed)
    for {
      (heldOutKey, _) <- featureMap
      otherFeatures = featureMap.filterKeys(_ != heldOutKey)
      shuffled: List[(K, F)] = random.shuffle(otherFeatures.toList)
      (train, dev) = shuffled.splitAt(numInTrain)
    } yield heldOutKey ->(train.toMap, dev.toMap)
  }

  def trainedClassifier[L](trainingData: Dataset[L, String], spans: Option[Iterable[(Int, Int)]] = None): Classifier[L,
    String] = {
    val mkClassifier: () => Classifier[L, String] = parameters.classifierType match {
      case SVM_L1 => () => new LiblinearClassifier(SolverType.L1R_L2LOSS_SVC, C = 1.0, eps = 0.01,
        bias = parameters.useBias)
      case SVM_L2 => () => new LiblinearClassifier(SolverType.L2R_L2LOSS_SVC, C = 1.0, eps = 0.01,
        bias = parameters.useBias)
      case RBF_SVM => () => new LibSVMClassifier[L, String](RBFKernel, C = 1.0)
      case Linear_SVM => () => new LibSVMClassifier[L, String](LinearKernel)
      case RandomForest => () => new RandomForestClassifier[L, String](
        numTrees = 7,
        featureSampleRatio = -1.0,
        // featuresToForce=featuresToForce,
        maxTreeDepth = parameters.maxTreeDepth.getOrElse(0))
    }
    val clf: Classifier[L, String] = parameters.baggingNClassifiers match {
      case None => mkClassifier()
      case Some(k) => new BaggingClassifier(mkClassifier, k)
    }
    clf.train(trainingData, spans)
    clf
  }

  // returns a trained classifier and a feature processing function
  def trainFromFeatures[L](trainingFeatures: Map[String, Counter[String]], trainingLabels: Map[String, L]): (Classifier[L, String], Counter[String] => Counter[String], Option[MutualInformation[L, String]]) = {
    val labels = trainingLabels.values.toSet

    val featureProcessor: CounterProcessor[String] = new CounterProcessor[String](trainingFeatures.values.toSeq,
      parameters.lexicalParameters.normalization, None, None)

    // normalize all features in the training set using the featureProcessor
    val normedFeatures: Map[String, Counter[String]] = trainingFeatures.mapValues(featureProcessor.apply)

    // if we've set a mutual information feature count threshold, only take those features with high MI
    val featureSelector: Option[MutualInformation[L, String]] = parameters.miNumToKeep.map(k =>
      new MutualInformation[L, String](
        normedFeatures.toSeq.map(_._2),
        normedFeatures.toSeq.map({ case (state, _) => trainingLabels(state) }),
        k,
        featuresToForce)
    )
    // apply the feature processing function to each counter
    val selectedFeatures: Map[String, Counter[String]] = featureSelector.map(fs => normedFeatures.mapValues(fs.apply)).getOrElse(normedFeatures)

    // bin feature values into number of quantiles specified in lexical parameters
    //binnedFeatures: Map[String, Counter[String]] = binVals(parameters.lexicalParameters.numFeatureBins.getOrElse(0))(selectedFeatures)

    // convert the map of states to features (selectedFeatures) into a dataset
    //trainingData: RVFDataset[L, String] = mkDataset({state: String => actualLabels.get(state)}, binnedFeatures)
    val trainingData: RVFDataset[L, String] = mkDataset({key: String => trainingLabels.get(key)}, selectedFeatures)

    val clf: Classifier[L, String] = trainedClassifier(trainingData)

    // create a function to process testing features by applying first normalization and then featureSelection,
    // if applicable
    val procFeats: (Counter[String] => Counter[String]) = { (counter: Counter[String]) =>
      val normed = featureProcessor(counter)
      featureSelector.map(fs => fs(normed)).getOrElse(normed)
    }
    return (clf, procFeats, featureSelector)
  }
}
