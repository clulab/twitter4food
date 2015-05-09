package edu.arizona.sista.twitter4food

import java.io.PrintWriter

import de.bwaldvogel.liblinear.SolverType
import edu.arizona.sista.learning._
import edu.arizona.sista.struct.Counter
import Mixins._

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
                                removeMarginals: Option[Int] = None,

                                // feature scaling factor: having tiny feature values leads to SVM numerical instability
                                featureScalingFactor: Option[Double] = Some(1000.0)) {
  override def toString = s"""tokenTypes: ${lexicalParameters.tokenTypes}
annotators: ${lexicalParameters.annotators}
regionType: ${regionType}
classifierType: ${classifierType}"""

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
    // scale each feature by the number of tweets aggregated (but divide by scaling factor to avoid SVM numerical instability)
    thresh / (tweets.size.toDouble / parameters.featureScalingFactor.getOrElse(1.0))
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

  def mkDataset[K, L](features: Seq[Counter[String]], labels: Seq[L]): RVFDataset[L, String] = {
    val dataset = new RVFDataset[L, String]
    for ((featureCounter, label) <- (features zip labels)) {
      dataset += new RVFDatum(label, featureCounter)
    }
    dataset
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
  def trainFromFeatures[L](features: Seq[Counter[String]], labels: Seq[L]): (Classifier[L, String], Counter[String] => Counter[String], Option[MutualInformation[L, String]]) = {
    val featureProcessor: CounterProcessor[String] = new CounterProcessor[String](features, parameters.lexicalParameters.normalization, None, None)

    // normalize all features in the training set using the featureProcessor
    val normedFeatures = features.map(featureProcessor.apply)

    // if we've set a mutual information feature count threshold, only take those features with high MI
    val featureSelector: Option[MutualInformation[L, String]] = parameters.miNumToKeep.map(k =>
      new MutualInformation[L, String](
        normedFeatures,
        labels,
        k,
        featuresToForce)
    )
    // apply the feature processing function to each counter
    val selectedFeatures: Seq[Counter[String]] = featureSelector.map(fs => normedFeatures.map(fs.apply)).getOrElse(normedFeatures)

    // bin feature values into number of quantiles specified in lexical parameters
    //binnedFeatures: Map[String, Counter[String]] = binVals(parameters.lexicalParameters.numFeatureBins.getOrElse(0))(selectedFeatures)

    // convert the map of states to features (selectedFeatures) into a dataset
    //dataset: RVFDataset[L, String] = mkDataset({state: String => actualLabels.get(state)}, binnedFeatures)
    val dataset: RVFDataset[L, String] = mkDataset(selectedFeatures, labels)

    val clf: Classifier[L, String] = trainedClassifier(dataset)

    // create a function to process testing features by applying first normalization and then featureSelection,
    // if applicable
    val procFeats: (Counter[String] => Counter[String]) = { (counter: Counter[String]) =>
      val normed = featureProcessor(counter)
      featureSelector.map(fs => fs(normed)).getOrElse(normed)
    }
    return (clf, procFeats, featureSelector)
  }
}

object Experiment {
  val geotagger = new GeoTagger

  def makeLabels(dataset: Map[String, Float], numClasses: Int, removeMarginals: Option[Int]) = {
    bin(numClasses)(normLocationsInMap(dataset, geotagger), removeMarginals)
  }

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

  def printWeights(p: java.io.PrintWriter, weightsByLabel: Map[Int, Counter[String]]) : Unit = {
    for ((label, weights) <- weightsByLabel) {
      p.println(label)
      def rep(list: Seq[(String, Double)]): Seq[String] =
        list.map({case (feature, weight) => "%s %.4f".format(feature, weight)})

      val sortedWeights = weights.sorted
      p.println("+\t-")
      p.println(rep(sortedWeights.take(20)).zip(rep(sortedWeights.reverse.take(20))).map({ case (s1, s2) => "%s\t%s".format(s1, s2) }).mkString("\n"))
      p.println
    }
  }

  def printWeights(p: java.io.PrintWriter, params: ExperimentParameters, counterMap: Map[String, Map[Int, Counter[String]]]) : Unit = {
    if (params.classifierType == SVM_L2 || params.classifierType == SVM_L1)  {
      p.println(params)

      for ((name, weightsByLabel) <- counterMap) {
        p.println(name.toUpperCase)
        printWeights(p, weightsByLabel)
        p.println
      }
      p.println
    }
  }

  def main(args: Array[String]) = {
    sys.error("Experiment has been moved to StateExperiment")
  }
}
