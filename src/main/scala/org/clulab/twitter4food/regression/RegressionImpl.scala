package org.clulab.twitter4food.regression

import org.clulab.learning._
import org.clulab.twitter4food.struct._
import org.clulab.twitter4food.util._
import java.io.{BufferedWriter, FileWriter}

import com.typesafe.config.{Config, ConfigFactory}
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import org.clulab.struct.Counter

/** Implementation of the Regression trait that contains the
  * nitty-gritty of creating FeatureExtractors, adding datums,
  * training and testing of regressionDatasets (with hyperparameter tuning).
  *
  * Specific subclassifiers would extend ClassifierImpl with fixed
  * configuration to tweak usage of unigrams, bigrams, topics, embeddings,
  * cosine similarity, and follower features.
  *
  * @author adikou
  * @author tishihara
  * @author Dane Bell
  */
class RegressionImpl(
                      val useUnigrams: Boolean,
                      val useBigrams: Boolean,
                      val useName: Boolean,
                      val useTopics: Boolean,
                      val useDictionaries: Boolean,
                      val useEmbeddings: Boolean,
//                      val useAvgEmbeddings: Boolean,
//                      val useMinEmbeddings: Boolean,
//                      val useMaxEmbeddings: Boolean,
                      val useCosineSim: Boolean,
                      val useLocation: Boolean,
                      val useTimeDate: Boolean,
//                      val useFoodPerc: Boolean,
//                      val useCaptions: Boolean,
                      val useFollowers: Boolean,
                      val useFollowees: Boolean,
                      val useRT: Boolean,
                      val useGender: Boolean,
                      val useAge: Boolean,
//                      val useRace: Boolean,
//                      val useHuman: Boolean,
                      val dictOnly: Boolean,
                      val denoise: Boolean,
                      val datumScaling: Boolean,
                      val featureScaling: Boolean,
                      val variable: String,
                      val customFeatures: (TwitterAccount) => Counter[String] = account => new Counter[String]()
                    ) extends Regression {

  import RegressionImpl._

  /** featureExtractor instance local to each classifier */
  val featureExtractor = new FeatureExtractor(
    useUnigrams=useUnigrams,
    useBigrams=useBigrams,
    useName=useName,
    useTopics=useTopics,
    useDictionaries=useDictionaries,
    useEmbeddings=useEmbeddings,
//    useAvgEmbeddings=useAvgEmbeddings,
//    useMinEmbeddings=useMinEmbeddings,
//    useMaxEmbeddings=useMaxEmbeddings,
    useCosineSim=useCosineSim,
    useLocation=useLocation,
    useTimeDate=useTimeDate,
//    useFoodPerc=useFoodPerc,
//    useCaptions=useCaptions,
    useFollowers=useFollowers,
    useFollowees=useFollowees,
    useRT=useRT,
    useGender=useGender,
    useAge=useAge,
//    useRace=useRace,
//    useHuman=useHuman,
    dictOnly=dictOnly,
    denoise=denoise,
    datumScaling=datumScaling,
    variable=variable,
    customFeatures=customFeatures)

  /** subRegression that does the actual training over {@link dataset} */
  var subRegression: Option[LiblinearRegression[String]] = None

  /** config file that fetches filepaths */
  val config: Config = ConfigFactory.load()
  val logger: Logger = LoggerFactory.getLogger(this.getClass)

  var scaleRange: Option[ScaleRange[String]] = None
  val lowerBound = 0.0
  val upperBound = 1.0

  def constructDataset(
                        accounts: Seq[TwitterAccount],
                        labels: Seq[Double],
                        followers: Option[Map[String, Seq[TwitterAccount]]],
                        followees: Option[Map[String, Seq[String]]]): RVFRegDataset[String] = {

    if (useFollowers && followers.nonEmpty) this.featureExtractor.setFollowers(followers.get)
    if (useFollowees && followees.nonEmpty) this.featureExtractor.setFollowees(followees.get)

    val dataset = new RVFRegDataset[String]()

    val pb = new me.tongfei.progressbar.ProgressBar("train()", 100)
    pb.start()
    pb.maxHint(accounts.size)
    pb.setExtraMessage("Populating...")

    // make datums
    val datums = ((accounts.toArray zip labels).par map {
      case (account, label) => {
        pb.step()
        // keep handle to sort with
        (account.id, featureExtractor.mkRegDatum(account, label))
      }
    }).seq.sortBy(_._1).unzip._2

    pb.stop()

    datums.foreach(datum => this.synchronized { dataset += datum })

    dataset
  }

  /**
    * Sequentially adds a [[RVFDatum]] of (label, mkDatum(account)), first loading followers/followees if necessary
    *
    * @param accounts
    * @param labels
    */
  def train(accounts: Seq[TwitterAccount], labels: Seq[Double]) = {
    val followers = if (useFollowers) Option(loadFollowers(accounts)) else None
    val followees = if (useFollowees) Option(loadFollowees(accounts, this.variable)) else None
    train(accounts, labels, followers, followees)
  }

  /** Sequentially adds a [[RVFDatum]] of (label, mkDatum(account))
    *
    * @param accounts: Sequence of training accounts
    * @param labels: Sequence of annotated labels for each account
    * @return Unit
    */
  def train(accounts: Seq[TwitterAccount],
            labels: Seq[Double],
            followers: Option[Map[String, Seq[TwitterAccount]]],
            followees: Option[Map[String, Seq[String]]]) = {
    assert(accounts.lengthCompare(labels.size) == 0)

    this.setRegression(Utils.svmRegressionFactory())

    // Clear current dataset if training on new one
    val dataset = constructDataset(accounts, labels, followers, followees)

    // normalize in place by feature (see FeatureExtractor for scaling by datum)
    // if (featureScaling) scaleRange = Some(Normalization.scaleByFeature(dataset, lowerBound, upperBound))

    // Train the classifier
    subRegression.get.train(dataset)
  }

  /** Essentially the test method, called by classOf method in FeatureExtractor
    * Populate datum for the test account and return the predicted scores
    * for each label.
    *
    * @param  account Test account
    * @return Counter[String] predicted scores for each label
    *         that classOf calls argmax on.
    */
  override def scoreOf(account: TwitterAccount): Double = {
    if(subRegression.isDefined) {
      val datum = featureExtractor.mkRegDatum(account, Double.MaxValue)
      // val scaled = if (featureScaling && scaleRange.nonEmpty)
      //   Normalization.scaleByRange(datum, scaleRange.get, lowerBound, upperBound)
      // else datum
      subRegression.get.scoreOf(datum)
    } else throw new RuntimeException("ERROR: must train before using scoreOf!")
  }

  /** Set custom classifier prior to training
    *
    * @param newRegression LiblinearRegression to use
    * @return Unit
    */
  def setRegression(newRegression: LiblinearRegression[String]) {
    subRegression = Some(newRegression)
  }

  /** Writes the predicted labels for each test account to file
    *
    * @param tests Sequence of test accounts
    * @param labels Sequence of predicted labels
    * @param file output filename
    */
  def saveTo(tests: Seq[TwitterAccount], labels: Seq[Double], file: String) = {
    val writer = new BufferedWriter(new FileWriter(file))

    for(i <- tests.indices) {
      writer.write(s"${tests(i).handle}\t${labels(i)}\n")
      writer.flush()
    }
    writer.close()
  }

  /*
  def featureSelectionIncremental(accounts: Map[TwitterAccount, Double],
                                  followers: Map[String, Seq[TwitterAccount]],
                                  followees: Map[String, Seq[String]],
                                  evalMetric: Iterable[(String, String)] => Double): Dataset[String, String] = {

    val dataset = constructDataset(accounts.keys.toSeq, accounts.values.toSeq, Option(followers), Option(followees))
    val featureGroups = Utils.findFeatureGroups(":", dataset.featureLexicon)
    logger.debug(s"Found ${featureGroups.size} feature groups:")
    for(f <- featureGroups.keySet) {
      logger.debug(s"Group $f containing ${featureGroups.get(f).get.size} features.")
    }
    val chosenGroups = Datasets.incrementalFeatureSelection[String, String](
      dataset, Utils.svmFactory, evalMetric, featureGroups)

    logger.info(s"Selected ${chosenGroups.size} feature groups: " + chosenGroups)

    dataset.keepOnly(chosenGroups.flatMap(g => featureGroups(g)))
  }

  def featureSelectionByFrequency(accounts: Map[TwitterAccount, String],
                                  followers: Map[String, Seq[TwitterAccount]],
                                  followees: Map[String, Seq[String]],
                                  evalMetric: Iterable[(String, String)] => Double): Dataset[String, String] = {
    val dataset = constructDataset(accounts.keys.toSeq, accounts.values.toSeq, Option(followers), Option(followees))
    val chosenFeatures = Datasets.featureSelectionByFrequency(dataset, Utils.svmFactory, evalMetric)
    dataset.keepOnly(chosenFeatures)
  }

  def featureSelectionIncremental(dataset:Dataset[String, String], evalMetric: Iterable[(String, String)] => Double): Dataset[String, String] = {
    val featureGroups = Utils.findFeatureGroups(":", dataset.featureLexicon)
    logger.debug(s"Found ${featureGroups.size} feature groups:")
    for(f <- featureGroups.keySet) {
      logger.debug(s"Group $f containing ${featureGroups(f).size} features.")
    }
    val chosenGroups = Datasets.incrementalFeatureSelection[String, String](
      dataset, Utils.svmFactory, evalMetric, featureGroups)

    logger.info(s"Selected ${chosenGroups.size} feature groups: " + chosenGroups)

    dataset.keepOnly(chosenGroups.flatMap(g => featureGroups(g)))
  }

  def featureSelectionIncrementalCV(
                                     dataset:Dataset[String, String],
                                     evalMetric: Iterable[(String, String)] => Double): (Dataset[String, String], Set[String]) = {
    val featureGroups = Utils.findFeatureGroups(":", dataset.featureLexicon)
    logger.debug(s"Found ${featureGroups.size} feature groups:")
    for(f <- featureGroups.keySet) {
      logger.debug(s"Group $f containing ${featureGroups(f).size} features.")
    }
    val chosenGroups = Datasets.incrementalFeatureSelection[String, String](
      dataset, Utils.svmFactory, evalMetric, featureGroups)

    logger.info(s"Selected ${chosenGroups.size} feature groups: " + chosenGroups)

    val reducedDataset = dataset.keepOnly(chosenGroups.flatMap(g => featureGroups(g)))

    (reducedDataset, chosenGroups)
  }

  def featureSelectionByFrequency(dataset:Dataset[String, String], evalMetric: Iterable[(String, String)] => Double): Dataset[String, String] = {
    val chosenFeatures = Datasets.featureSelectionByFrequency(dataset, Utils.svmFactory, evalMetric)
    dataset.keepOnly(chosenFeatures)
  }
  */

  /**
    * Implements stratified cross validation; producing pairs of gold/predicted labels across the training dataset.
    * Each fold is as balanced as possible by label L. Returns the weights of each classifier in addition to predictions.
    */
  def cv(
          accounts: Seq[TwitterAccount],
          labels: Seq[Double],
          partitions: Map[Long, Int],
          portion: Double = 1.0,
          followers: Option[Map[String, Seq[TwitterAccount]]] = None,
          followees: Option[Map[String, Seq[String]]] = None,
          regressionFactory: () => LiblinearRegression[String]
              ): (Seq[(Double, Double)], Seq[(String, Double)]) = {

    assert(accounts.length == labels.length, "Number of accounts and labels must be equal")

    // for printing out feature weights (including for specific account classifications)
    val numFeatures = 30
    val numAccts = 20

    // Important: this dataset is sorted by id
    val ids = partitions.keys.toSeq.sorted
    val dataset = constructDataset(accounts, labels, followers, followees)
    val folds = TrainTestFold.foldsFromIds(ids, partitions, portion)

    val results = for (fold <- folds) yield {
      logger.debug(s"train:${fold.train.length}; test:${fold.test.length}; overlap:${fold.train.toSet.intersect(fold.test.toSet).size}")
      if(logger.isDebugEnabled) {
        val balance = fold.test.map(dataset.labels(_)).groupBy(identity).mapValues(_.size)
        logger.debug(s"fold: ${balance.mkString(", ")}")
      }
      val regression = regressionFactory()
      regression.train(dataset, fold.train.toArray)
      val W = regression.getWeights()
      val predictions = for(i <- fold.test) yield {
        val id = ids(i).toString
        val gold = dataset.labels(i)
        val datum = dataset.mkDatum(i)
        val score = regression.scoreOf(datum)
        (id, datum, gold, score)
      }

      (W, predictions)
    }

    val allFeats = dataset.featureLexicon.keySet
    val (allWeights, predictions) = results.unzip
    val g = predictions.flatten.map(_._3)
    val p = predictions.flatten.map(_._4)
    val evalInput = g.zip(p)

    val avgWeights = new Counter[String]
    allFeats.foreach(k => avgWeights.setCount(k, allWeights.map(W => W.getCount(k)).sum))
    avgWeights.mapValues(_ / allWeights.length)

    val topWeights = avgWeights.sorted.take(numFeatures)

    (evalInput, topWeights)
  }


  /**
    * Feature selection using stratified cross validation; producing pairs of gold/predicted labels across the training dataset.
    * Each fold is as balanced as possible by label L. Returns selected features with predictions to help estimate F1.
    */
  /*
  def fscv(
            accounts: Seq[TwitterAccount],
            labels: Seq[Double],
            partitions: Map[Long, Int],
            followers: Option[Map[String, Seq[TwitterAccount]]],
            followees: Option[Map[String, Seq[String]]],
            regressionFactory: () => LiblinearRegression[String],
            evalMetric: Iterable[(String, String)] => Double
          ): Seq[(String, String)] = {

    // Important: this dataset is sorted by id
    val dataset = constructDataset(accounts, labels, followers, followees)
    val ids = accounts.map(_.id).sorted
    val folds = TrainDevTestFold.devFoldsFromIds(ids, partitions)

    val results = for {
      fold <- folds
    } yield {
      if(logger.isDebugEnabled) {
        val balance = fold.test.map(dataset.labels(_)).groupBy(identity).mapValues(_.size)
        logger.debug(s"fold: ${balance.mkString(", ")}")
      }
      val (tunedDataset, selectedFeatures) = featureSelectionIncrementalCV(Utils.keepRows(dataset, fold.train.toArray), evalMetric)
      val classifier = regressionFactory()
      classifier.train(tunedDataset)
      val predictions = for(i <- fold.dev) yield {
        val gold = dataset.labelLexicon.get(dataset.labels(i))
        val datum = dataset.mkDatum(i)
        val pred = classifier.classOf(datum)
        (gold, pred)
      }
      selectedFeatures -> predictions
    }

    // We'll select the same number of features as the most selected by any one fold's fs process
    // This will tend to make bigger feature sets with more folds, but there's no principled way to do this AFAIK
    val maxFeats = results.unzip._1.map(_.size).max

    // Every time a feature set is selected for a fold, it gets a vote equal to that fold's best F1
    val scoreBoard = scala.collection.mutable.Map[String, Double]()
    results.foreach{ case (features, preds) =>
      val (evalMeasures, microAvg, macroAvg) = Eval.evaluate(preds)
      val f1 = evalMeasures(labelSet("pos")).F
      features.foreach(f => scoreBoard(f) = scoreBoard.getOrElse(f,0.0) + f1)
    }
    scoreBoard.toMap.foreach{ case (feature, f1) => logger.info(f"$feature: $f1%1.3f") }

    // We select featureSets based on those votes
    val selected = scoreBoard.toSeq.sortBy(_._2).takeRight(maxFeats).map(_._1).toSet

    logger.info(s"Final selection: ${selected.mkString(", ")}")

    // We trim our dataset to contain only the selected features
    val featureGroups = Utils.findFeatureGroups(":", dataset.featureLexicon)
    val reducedDS = dataset.keepOnly(selected.flatMap(g => featureGroups(g)))

    // We pick the head arbitrarily because all these folds have the same 'test' fold
    val trainIndices = (folds.head.train ++ folds.head.dev).toArray
    val finalClassifier = regressionFactory()

    // Then we train a final classifier on all but the test fold,
    // and make predictions on the previously unseen test fold
    finalClassifier.train(Utils.keepRows(reducedDS, trainIndices))
    val predictions = for(i <- folds.head.test) yield {
      val gold = reducedDS.labelLexicon.get(reducedDS.labels(i))
      val datum = reducedDS.mkDatum(i)
      val pred = finalClassifier.classOf(datum)
      (gold, pred)
    }

    predictions
  }
  */
}

object RegressionImpl {
  /** config file that fetches filepaths */
  val config = ConfigFactory.load()

  val logger = LoggerFactory.getLogger(this.getClass)

  def loadFollowees(accounts: Seq[TwitterAccount], variable: String): Map[String, Seq[String]] = {
    val followeeFile = scala.io.Source.fromFile(config.getString(s"regressions.$variable.followeeRelations"))
    val handleToFollowees = (for (line <- followeeFile.getLines) yield {
      val handles = line.split("\t+")
      handles.head -> handles.tail.toSeq
    }).toMap
    followeeFile.close

    handleToFollowees
  }

  def loadFollowers(accounts: Seq[TwitterAccount]): Map[String, Seq[TwitterAccount]] = {
    val followerFile = scala.io.Source.fromFile(config.getString("regressions.features.followerRelations"))
    val handleToFollowers: Map[String, Seq[String]] = (for (line <- followerFile.getLines) yield {
      val handles = line.split("\t")
      handles.head -> handles.tail.toSeq
    }).toMap
    followerFile.close

    val accountsFileStr = config.getString("regressions.features.followerAccounts")
    val followerAccounts = FileUtils.loadTwitterAccounts(accountsFileStr)
    val handleToFollowerAccts = accounts.map{ account =>
      val followerAccount = handleToFollowers.getOrElse(account.handle, Nil).flatMap { followerHandle =>
        followerAccounts.find{ case(f, label) => f.handle == followerHandle }
      }.map(_._1)
      account.handle -> followerAccount
    }.toMap

    handleToFollowerAccts
  }

  def outputAnalysis(outputDir: String,
                     weights: Map[String, Seq[(String, Double)]]): Unit = {

    val numWeights = 30

    val barrier = "=" * 25
    // Initialize writer
    val writer = new BufferedWriter(new FileWriter(outputDir + "/weights.txt", false))
    weights.foreach { case (label, feats) =>
      writer.write(s"$label weights\n$barrier\n")
      writer.write(feats.take(numWeights).map(feat => s"${feat._1}\t${feat._2}").mkString("\n"))
      writer.write("\n")
    }
    writer.close()
  }

  def outputAnalysis(outputDir: String,
                     weights: Seq[(String, Double)]): Unit = {

    val numWeights = 30

    // Initialize writer
    val writer = new BufferedWriter(new FileWriter(outputDir + "/weights.txt", false))

    val sortedWeights = weights.sortBy(_._2)
    if (numWeights * 2 >= weights.length) {
      val printable = weights.map(feat => s"${feat._1}\t${feat._2}").mkString("\n")
      writer.write(printable)
    } else {
      val mostNegative = weights.take(numWeights).map(feat => s"${feat._1}\t${feat._2}").mkString("\n")
      val mostPositive = weights.takeRight(numWeights).map(feat => s"${feat._1}\t${feat._2}").mkString("\n")

      writer.write(mostNegative)
      writer.write("\n...\n")
      writer.write(mostPositive)
    }

    writer.close()
  }

  def fMeasure(precision: Double, recall: Double, beta: Double): Double =
    (1 + Math.pow(beta, 2)) * ((precision * recall) / (Math.pow(beta, 2) * precision + recall))
}