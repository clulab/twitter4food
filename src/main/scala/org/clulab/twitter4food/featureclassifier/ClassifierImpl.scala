package org.clulab.twitter4food.featureclassifier

import org.clulab.learning._
import org.clulab.struct.Counter
import org.clulab.twitter4food.struct._
import org.clulab.twitter4food.util._
import java.io.{BufferedWriter, FileWriter}

import com.typesafe.config.{Config, ConfigFactory}
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import org.clulab.struct.Counter
import org.clulab.struct.Counter

/** Implementation of the FeatureClassifier trait that contains the
  * nitty-gritty of creating FeatureExtractors, adding datums,
  * training and testing of datasets (with hyperparameter tuning).
  *
  * Specific subclassifiers would extend ClassifierImpl with fixed
  * configuration to tweak usage of unigrams, bigrams, topics, embeddings,
  * cosine similarity, and follower features. 
  *
  * @author adikou
  * @author tishihara
  * @author Dane Bell
  */

/** Used by Stratified K-fold CV */
case class TrainTestFold(test: Seq[Int], train: Seq[Int]) {
  def merge(other: TrainTestFold): TrainTestFold = {
    new TrainTestFold(this.test ++ other.test, this.train ++ other.train)
  }
}

/** Used by Stratified K-fold CV */
case class TrainDevTestFold(test: Seq[Int], dev: Seq[Int], train: Seq[Int]) {
  def merge(other: TrainDevTestFold): TrainDevTestFold = {
    new TrainDevTestFold(this.test ++ other.test, this.dev ++ other.dev, this.train ++ other.train)
  }
}

class ClassifierImpl(
  val useUnigrams: Boolean,
  val useBigrams: Boolean,
  val useName: Boolean,
  val useTopics: Boolean,
  val useDictionaries: Boolean,
  val useAvgEmbeddings: Boolean,
  val useMinEmbeddings: Boolean,
  val useMaxEmbeddings: Boolean,
  val useCosineSim: Boolean,
  val useLocation: Boolean,
  val useTimeDate: Boolean,
  val useFoodPerc: Boolean,
  val useCaptions: Boolean,
  val useFollowers: Boolean,
  val useFollowees: Boolean,
  val useRT: Boolean,
  val useGender: Boolean,
  val useAge: Boolean,
  val useRace: Boolean,
  val useHuman: Boolean,
  val dictOnly: Boolean,
  val denoise: Boolean,
  val datumScaling: Boolean,
  val featureScaling: Boolean,
  val variable: String,
  val customFeatures: (TwitterAccount) => Counter[String] = account => new Counter[String]()
) extends FeatureClassifier {

  import ClassifierImpl._

  /** featureExtractor instance local to each classifier */
  val featureExtractor = new FeatureExtractor(
    useUnigrams=useUnigrams,
    useBigrams=useBigrams,
    useName=useName,
    useTopics=useTopics,
    useDictionaries=useDictionaries,
    useAvgEmbeddings=useAvgEmbeddings,
    useMinEmbeddings=useMinEmbeddings,
    useMaxEmbeddings=useMaxEmbeddings,
    useCosineSim=useCosineSim,
    useLocation=useLocation,
    useTimeDate=useTimeDate,
    useFoodPerc=useFoodPerc,
    useCaptions=useCaptions,
    useFollowers=useFollowers,
    useFollowees=useFollowees,
    useRT=useRT,
    useGender=useGender,
    useAge=useAge,
    useRace=useRace,
    useHuman=useHuman,
    dictOnly=dictOnly,
    denoise=denoise,
    datumScaling=datumScaling,
    variable=variable,
    customFeatures=customFeatures)

  /** subClassifier that does the actual training over {@link dataset} */
  var subClassifier: Option[LiblinearClassifier[String, String]] = None

  /** config file that fetches filepaths */
  val config: Config = ConfigFactory.load()
  val logger: Logger = LoggerFactory.getLogger(this.getClass)

  var scaleRange: Option[ScaleRange[String]] = None
  val lowerBound = 0.0
  val upperBound = 1.0

  def constructDataset(
    accounts: Seq[TwitterAccount],
    labels: Seq[String],
    followers: Option[Map[String, Seq[TwitterAccount]]],
    followees: Option[Map[String, Seq[String]]]): RVFDataset[String, String] = {

    if (useFollowers && followers.nonEmpty) this.featureExtractor.setFollowers(followers.get)
    if (useFollowees && followees.nonEmpty) this.featureExtractor.setFollowees(followees.get)

    val dataset = new RVFDataset[String, String]()

    val pb = new me.tongfei.progressbar.ProgressBar("train()", 100)
    pb.start()
    pb.maxHint(accounts.size)
    pb.setExtraMessage("Populating...")

    // make datums
    val datums = ((accounts.toArray zip labels).par map {
      case (account, label) => {
        pb.step()
        // keep handle to sort with
        (account.id, featureExtractor.mkDatum(account, label))
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
  def train(accounts: Seq[TwitterAccount], labels: Seq[String]) = {
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
    labels: Seq[String],
    followers: Option[Map[String, Seq[TwitterAccount]]],
    followees: Option[Map[String, Seq[String]]]) = {
    assert(accounts.size == labels.size)

    this.setClassifier(new L1LinearSVMClassifier[String, String]())

    // Clear current dataset if training on new one
    val dataset = constructDataset(accounts, labels, followers, followees)

    // normalize in place by feature (see FeatureExtractor for scaling by datum)
    if (featureScaling) scaleRange = Some(Normalization.scaleByFeature(dataset, lowerBound, upperBound))

    // Train the classifier
    subClassifier.get.train(dataset)
  }

  /** Essentially the test method, called by classOf method in FeatureExtractor
    * Populate datum for the test account and return the predicted scores
    * for each label.
    *
    * @param  account Test account
    * @return Counter[String] predicted scores for each label
    *         that classOf calls argmax on.
    */
  override def scoresOf(account: TwitterAccount): Counter[String] = {
    if(subClassifier.isDefined) {
      val datum = featureExtractor.mkDatum(account, "unknown")
      val scaled = if (featureScaling && scaleRange.nonEmpty)
        Normalization.scaleByRange(datum, scaleRange.get, lowerBound, upperBound)
      else datum
      subClassifier.get.scoresOf(scaled)
    } else throw new RuntimeException("ERROR: must train before using scoresOf!")
  }

  /** Set custom classifier prior to training
    *
    * @param newClassifier LiblinearClassifier to use
    * @return Unit
    */
  def setClassifier(newClassifier: LiblinearClassifier[String, String]): Unit = {
    subClassifier = Some(newClassifier)
  }

  /** Part 1/3 of test suite associated with each classifier. Resets the
    * subClassifier with new hyperparameter C, fetches top-K tweets for
    * each account in trainingSet and calls {@link train} method over
    * the modified trainingSet.
    *
    * @param trainingSet sequence of twitter accounts to train on
    * @param trainingLabels sequence of annotated labels for each account
    * @param _C hyperparameter for current run of subClassifier
    * @param K parameter for slicing the top-K tweets of each account.
    * @param args Used for directing the filename for the model file
    * @return Unit
    */
  def _train(trainingSet: Seq[TwitterAccount],
    trainingLabels: Seq[String],
    _C: Double,
    K: Int,
    args: Array[String]) = {

    subClassifier = Some(new L1LinearSVMClassifier[String, String](C=_C))

    // Skim only top-K tweets for each account
    val customAccounts = trainingSet.map(t => {
      val numTweets = Math.min(K, t.tweets.size)
      new TwitterAccount(t.handle, t.id, t.name, t.lang, t.url,
        t.location, t.description, t.tweets.slice(0, numTweets),
        Seq[TwitterAccount]())
    })

    val opt = config.getString(s"classifiers.${this.variable}.model")
    val nonFeats = Seq("--analysis", "--test", "--noTraining", "--learningCurve")
    val fout = s"$opt/svm_${args.filterNot(nonFeats.contains).sorted.mkString("").replace("-", "")}_${_C}_$K.dat"

    logger.info(s"Training on ${customAccounts.length} accounts, ${customAccounts.map(_.tweets.length).sum} tweets")

    // Train with top K tweets
    train(customAccounts, trainingLabels)
    subClassifier.get.saveTo(fout)
  }

  /** Part 2/3 of test suite. After calling {@link _train} method, _test
    * predicts the label for each twitter account in testSet.
    *
    * @param testSet sequence of twitter accounts to predict labels on
    * @return predictedLabels sequence of predicted labels
    */
  def _test(testSet: Seq[TwitterAccount]): Seq[String] = {

    // Don't bother with print statements for only one account
    val plural = testSet.length > 1
    if (plural) logger.info(s"Testing on ${testSet.length} accounts, ${testSet.map(_.tweets.length).sum} tweets")

    val pb = if (plural) Some(new me.tongfei.progressbar.ProgressBar("runTest()", 100)) else None
    if (plural) {
      pb.get.start()
      pb.get.maxHint(testSet.length)
      pb.get.setExtraMessage("Predicting...")
    }

    val predictedLabels = testSet.map{u =>
      val label = classify(u)
      if (plural) pb.get.step()
      label
    }

    if (plural) pb.get.stop()

    predictedLabels
  }

  /** Part 3/3 of test suite. Following calls to {@link _train} and
    * {@link _test}, _evaluate prints and writes to file, the F-1 scores
    * and (Precision, Recall, Accuracy, F-1 score) for each label, along
    * with macro- and micro- averages for the system.
    *
    * @param testingLabels sequence of source labels
    * @param predictedLabels sequence of target labels
    * @param testSet sequence of test accounts to track wrong predictions
    * @param writer BufferedWriter to write output to file
    * @param _C subClassifier hyperparameter
    * @param K threshold for top-K tweets
    * @return (evalMeasures, microAvg, macroAvg) tuple to track performance
    */
  def _evaluate(testingLabels: Seq[String],
    predictedLabels: Seq[String],
    testSet: Seq[TwitterAccount],
    writer: BufferedWriter, _C: Double, K: Int) = {
    val (evalMeasures, microAvg, macroAvg) = Eval.evaluate(testingLabels,
      predictedLabels, testSet)

    val df = new java.text.DecimalFormat("#.###")

    println(s"C=${_C}, #K=$K")
    println(evalMeasures.mkString("\n"))
    println(s"\nMacro avg F-1 : ${df.format(macroAvg)}")
    println(s"Micro avg F-1 : ${df.format(microAvg)}")
    writer.write(s"C=${_C}, #K=$K\n")
    writer.write(evalMeasures.mkString("\n"))
    evalMeasures.keys.foreach(l => {
      writer.write(s"\nl\nFP:\n")
      writer.write(s"${evalMeasures(l).FPAccounts.map(u => u.handle).mkString("\n")}\nFN:\n")
      writer.write(s"${evalMeasures(l).FNAccounts.map(u => u.handle).mkString("\n")}\n")
    })
    writer.write(s"\nMacro avg F-1 : ${df.format(macroAvg)}\n")
    writer.write(s"Micro avg F-1 : ${df.format(microAvg)}\n")
    writer.flush()

    (evalMeasures, microAvg, macroAvg)
  }

  /** Pick one of (C, K) and store in gridCbyK and pick (C,,max,,, K,,max,,)
    * such that accuracy for that micro-average for that tuple in dev set
    * is maximum. Use the max values and train with trainSet++devSet and test
    * with testSet.
    *
    * @param args command line arguments for specifying output file name
    * @param ctype classifier type: "gender", "human", "overweight" etc.
    * @param outputFile filename to direct output to
    * @return Unit
    */
  def runTest(args: Array[String], ctype: String, outputFile: String = null, devOnly: Boolean = true) = {

    println("Loading training accounts...")
    val trainingData = FileUtils.loadTwitterAccounts(config
      .getString(s"classifiers.$ctype.trainingData"))
    println("Loading dev accounts...")
    val devData = FileUtils.loadTwitterAccounts(config
      .getString(s"classifiers.$ctype.devData"))
    println("Loading test accounts...")
    val testData = if(!devOnly) Some(FileUtils.loadTwitterAccounts(config.getString(s"classifiers.$ctype.testData"))) else None

    val fileExt = args.mkString("").replace("-", "").sorted
    val tweetFolds = Array(100, 500, 1000, 5000)
    val cFolds = Array(0.001, 0.1, 10, 1000)

    /*
     * (i)  Tune parameters
     * (ii) Pick top-K tweets
     */

    val (trainUsers, devUsers, testUsers) = (trainingData.keys.toArray,
      devData.keys.toArray, testData.getOrElse(Map()).keys.toArray)

    val (trainLabels, devLabels, testLabels) = (trainingData.values.toArray,
      devData.values.toArray, testData.getOrElse(Map()).values.toArray)

    val writerFile = if (outputFile != null) outputFile
    else config.getString("classifier") + s"/$ctype/output-" +
      fileExt + ".txt"

    val writer = new BufferedWriter(new FileWriter(
      writerFile, true))

    // Values for accuracies for C * K
    val gridCbyK = Array.ofDim[Double](4,4)

    /** For a given classifier, loadTwitterAccounts its associated train, dev, and test
      * accounts, and write results to file.
      *
      * @return microAvg micro-average aggregated over each label
      */
    val unitTest = (trainingSet: Seq[TwitterAccount],
    trainingLabels: Seq[String],
    testingSet: Seq[TwitterAccount],
    testingLabels: Seq[String],
    _C: Double,
    K: Int) => {

      println(s"Training with C=${_C} and top-$K tweets")

      _train(trainingSet, trainingLabels,_C, K, args)
      val predictedLabels = _test(testingSet)

      val (evalMeasures, microAvg, macroAvg) = _evaluate(testingLabels,
        predictedLabels, testingSet, writer, _C, K)

      microAvg
    }

    for(i <- cFolds.indices) {
      val _C = cFolds(i)
      for(j <- tweetFolds.indices) {
        val K = tweetFolds(j)
        gridCbyK(i)(j) = unitTest(trainUsers, trainLabels,
          devUsers, devLabels, _C, K)
      }
    }

    var (iMax, jMax) = (0,0)
    var max = Double.MinValue
    for(i <- gridCbyK.indices)
      for(j <- gridCbyK(i).indices)
        if(gridCbyK(i)(j) > max) {
          max = gridCbyK(i)(j)
          iMax = i
          jMax = j
        }

    println(s"Best C = ${cFolds(iMax)}, Top K = ${tweetFolds(jMax)}")
    writer.write(s"Best C = ${cFolds(iMax)}, Top K = ${tweetFolds(jMax)}\n")

    if (!devOnly) {
      println("Testing with test users")
      writer.write("*****Test*****\n")
      writer.flush()

      // Final run on test Set
      unitTest(trainUsers ++ devUsers, trainLabels ++ devLabels,
        testUsers, testLabels, cFolds(iMax), tweetFolds(jMax))

      println("*****Test complete*****")
      writer.write("*****Test complete*****\n")
    }
    writer.flush()

    writer.close()
  }

  /** Training over all data: train, dev, and test as one. Used for
    * predicting labels for new unlabeled accounts
    *
    * @param args Options for selectin features
    * @param ctype Classifier type: "gender", "age", "race" etc.
    * @param _C hyperparameter
    * @param K threshold for top-K tweets
    */
  def learn(args: Array[String], ctype: String, _C: Double, K: Int) = {
    val allTrainData = FileUtils.loadTwitterAccounts(config.getString(
      s"classifiers.$ctype.allTrainData"))

    val allTrainAccounts = allTrainData.keys.toArray
    val allTrainLabels = allTrainData.values.toArray

    _train(allTrainAccounts, allTrainLabels, _C, K, args)

  }

  /** Predict labels after calling {@link learn}
    *
    * @param test/tests/testFile Load from file, or input sequence of tests
    * @return Seq[String] predicted labels
    */
  def predict(test: TwitterAccount) = _test(Array(test)).head

  def predict(tests: Seq[TwitterAccount]) = _test(tests)

  def predict(testFile: String) = {

    val allTestData = FileUtils.loadTwitterAccounts(testFile)
    val testAccounts = allTestData.keys.toArray

    val predictedLabels = _test(testAccounts)
  }

  /** Writes the predicted labels for each test account to file
    *
    * @param tests Sequence of test accounts
    * @param labels Sequence of predicted labels
    * @param file output filename
    */
  def saveTo(tests: Seq[TwitterAccount], labels: Seq[String], file: String) = {
    val writer = new BufferedWriter(new FileWriter(file))

    for(i <- tests.indices) {
      writer.write(s"${tests(i).handle}\t${labels(i)}\n")
      writer.flush()
    }
    writer.close()
  }

  def featureSelectionIncremental(accounts: Map[TwitterAccount, String],
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

  /**
    * Returns a [[Seq]] of [[TrainTestFold]]s given unique ids and a map of what partition they belong in.
    */
  def foldsFromIds(ids: Seq[Long], partitions: Map[Long, Int]): Seq[TrainTestFold] = {
    val numPartitions = partitions.values.max
    val allIndices = ids.indices.toSet
    val idxToFold = for ((id, idx) <- ids.zipWithIndex) yield {
      idx -> partitions(id)
    }
    val foldToIndices = idxToFold.groupBy(_._2).map{ case (p, is) => p -> is.map(_._1).toSet }
    for (p <- 0 until numPartitions) yield {
      TrainTestFold(foldToIndices(p).toSeq, (allIndices -- foldToIndices(p)).toSeq)
    }
  }

  /**
    * Returns a [[Seq]] of [[TrainDevTestFold]]s given unique ids and a map of what partition they belong in.
    * The test fold is the same for all [[TrainDevTestFold]]s to maintain the independence of test throughout feature
    * selection.
    */
  def devFoldsFromIds(ids: Seq[Long], partitions: Map[Long, Int]): Seq[TrainDevTestFold] = {
    val lastPartition = partitions.values.max
    val allIndices = ids.indices.toSet
    val idxToFold = for ((id, idx) <- ids.zipWithIndex) yield {
      idx -> partitions(id)
    }
    val foldToIndices = idxToFold.groupBy(_._2).map{ case (p, is) => p -> is.map(_._1).toSet }
    val test = foldToIndices(lastPartition) // the test partition will be the same in all cases
    for (p <- 0 until lastPartition) yield {
      TrainDevTestFold(foldToIndices(p).toSeq, (allIndices -- foldToIndices(p) -- test).toSeq, test.toSeq)
    }
  }

  /** Creates dataset folds to be used for cross validation */
  def mkStratifiedTrainTestFolds[L, F](
    numFolds:Int,
    dataset:Dataset[L, F],
    seed:Int
  ): Iterable[TrainTestFold] = {
    val r = new Random(seed)

    val byClass: Map[Int, Seq[Int]] = r.shuffle[Int, IndexedSeq](dataset.indices).groupBy(idx => dataset.labels(idx))
    val folds = (for (i <- 0 until numFolds) yield (i, new ArrayBuffer[TrainTestFold])).toMap

    for {
      c <- 0 until dataset.numLabels
      i <- 0 until numFolds
    } {
      val cds = byClass(c)
      val classSize = cds.length
      val foldSize = classSize / numFolds
      val startTest = i * foldSize
      val endTest = if (i == numFolds - 1) math.max(classSize, (i + 1) * foldSize) else (i + 1) * foldSize

      val trainFolds = new ArrayBuffer[Int]
      if(startTest > 0)
        trainFolds ++= cds.slice(0, startTest)
      if(endTest < classSize)
        trainFolds ++= cds.slice(endTest, classSize)

      folds(i) += new TrainTestFold(cds.slice(startTest, endTest), trainFolds)
    }
    folds.map{ dsfSet => dsfSet._2.reduce(_ merge _) }
  }

  /** Creates dataset folds to be used for cross validation */
  def mkStratifiedTrainDevTestFolds[L, F](
    numFolds:Int,
    dataset:Dataset[L, F],
    seed:Int
  ): Iterable[TrainDevTestFold] = {
    val r = new Random(seed)

    val byClass: Map[Int, Seq[Int]] = r.shuffle[Int, IndexedSeq](dataset.indices).groupBy(idx => dataset.labels(idx))
    val folds = (for (i <- 0 until numFolds) yield (i, new ArrayBuffer[TrainDevTestFold])).toMap

    for {
      c <- 0 until dataset.numLabels
      i <- 0 until numFolds
      j = (i + 1) % numFolds
    } {
      val cds = byClass(c)
      val classSize = cds.length
      val foldSize = classSize / numFolds
      val startTest = i * foldSize
      val endTest = if (i == numFolds - 1) math.max(classSize, (i + 1) * foldSize) else (i + 1) * foldSize
      val startDev = j * foldSize
      val endDev = if (j == numFolds - 1) math.max(classSize, (j + 1) * foldSize) else (j + 1) * foldSize

      val nonTrain = (startTest until endTest) ++ (startDev until endDev)
      val trainFolds = cds.indices.filterNot(ix => nonTrain.contains(ix))

      folds(i) += new TrainDevTestFold(cds.slice(startTest, endTest), cds.slice(startDev, endDev), trainFolds.map(cds.apply))
    }
    folds.map{ dsfSet => dsfSet._2.reduce(_ merge _) }
  }

  /**
    * Implements stratified cross validation; producing pairs of gold/predicted labels across the training dataset.
    * Each fold is as balanced as possible by label L.
    */
  def stratifiedCrossValidate[L, F](
    dataset:Dataset[L, F],
    classifierFactory: () => Classifier[L, F],
    numFolds:Int = 10,
    seed:Int = 73
  ): Seq[(L, L)] = {

    val folds = mkStratifiedTrainTestFolds(numFolds, dataset, seed)

    val output = for (fold <- folds) yield {
      if(logger.isDebugEnabled) {
        val balance = fold.test.map(dataset.labels(_)).groupBy(identity).mapValues(_.size)
        logger.debug(s"fold: ${balance.mkString(", ")}")
      }
      val classifier = classifierFactory()
      classifier.train(dataset, fold.train.toArray)
      for(i <- fold.test) yield {
        val gold = dataset.labelLexicon.get(dataset.labels(i))
        val pred = classifier.classOf(dataset.mkDatum(i))
        (gold, pred)
      }
    }

    output.flatten.toSeq
  }

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
    classifierFactory: () => LiblinearClassifier[String, String],
    percentTopToConsider: Double = 1.0
  ): (Seq[(String, String)],
    Map[String, Seq[(String, Double)]],
    Seq[(String, Map[String, Seq[(String, Double)]])],
    Seq[(String, Map[String, Seq[(String, Double)]])]) = {

    val numFeatures = 30
    val numAccts = 20

    // Important: this dataset is sorted by id
    val dataset = constructDataset(accounts, labels, followers, followees)
    val ids = accounts.map(_.id).sorted
    val folds = foldsFromIds(ids, partitions)

    val results = for (fold <- folds) yield {
      if(logger.isDebugEnabled) {
        val balance = fold.test.map(dataset.labels(_)).groupBy(identity).mapValues(_.size)
        logger.debug(s"fold: ${balance.mkString(", ")}")
      }
      val classifier = classifierFactory()
      classifier.train(dataset, fold.train.toArray)
      val W = classifier.getWeights()
      val predictions = for(i <- fold.test) yield {
        val id = ids(i).toString
        val gold = dataset.labelLexicon.get(dataset.labels(i))
        val datum = dataset.mkDatum(i)
        val pred = classifier.classOf(datum)
        val score = classifier.scoresOf(datum)
        // NOTE: for the high confidence classifier, sort this tuple in decreasing order of classifier confidence ('score(pred)')
        //    and take the top x percent (x is a parameter)
        (id, gold, pred, datum, score, score.getCount(pred))
      }

      val highConfPredictions = predictions.sortBy(- _._6).take( (percentTopToConsider * predictions.size).toInt )
      (W, highConfPredictions)
    }

    val allFeats = dataset.featureLexicon.keySet
    val (allWeights, predictions) = results.unzip
    val g = predictions.flatten.map(_._2)
    val p = predictions.flatten.map(_._3)
    val evalInput = g.zip(p)

    val avgWeights = (for {
      l <- dataset.labelLexicon.keySet
    } yield {
      val c = new Counter[String]
      allFeats.foreach(k => c.setCount(k, allWeights.map(W => W(l).getCount(k)).sum))
      c.mapValues(_ / allWeights.length)
      l -> c
    }).toMap

    val topWeights = avgWeights.mapValues(feats => feats.sorted.take(numFeatures))

    val pToW = (for {
      i <- results.indices
      p <- results(i)._2
    } yield p -> i).toMap

    val owScale = predictions
      .flatten
      .filter(acct => acct._2 != "Overweight" && acct._3 == "Overweight") // only false positives
      .sortBy(_._5.getCount("Overweight"))
      .reverse
      .take(numAccts)
    val noScale = predictions
      .flatten
      .filter(acct => acct._2 == "Overweight" && acct._3 != "Overweight") // only false negatives
      .sortBy(_._5.getCount("Not overweight"))
      .reverse
      .take(numAccts)

    val falsePos = owScale.map(acct => acct._1 -> Utils.analyze(allWeights(pToW(acct)), acct._4))
    val falseNeg = noScale.map(acct => acct._1 -> Utils.analyze(allWeights(pToW(acct)), acct._4))

    (evalInput, topWeights, falsePos, falseNeg)
  }


  /**
    * Feature selection using stratified cross validation; producing pairs of gold/predicted labels across the training dataset.
    * Each fold is as balanced as possible by label L. Returns selected features with predictions to help estimate F1.
    */
  def fscv(
    accounts: Seq[TwitterAccount],
    labels: Seq[String],
    partitions: Map[Long, Int],
    followers: Option[Map[String, Seq[TwitterAccount]]],
    followees: Option[Map[String, Seq[String]]],
    classifierFactory: () => LiblinearClassifier[String, String],
    evalMetric: Iterable[(String, String)] => Double
  ): Seq[(String, String)] = {

    // Important: this dataset is sorted by id
    val dataset = constructDataset(accounts, labels, followers, followees)
    val ids = accounts.map(_.id).sorted
    val folds = devFoldsFromIds(ids, partitions)

    val results = for {
      fold <- folds
    } yield {
      if(logger.isDebugEnabled) {
        val balance = fold.test.map(dataset.labels(_)).groupBy(identity).mapValues(_.size)
        logger.debug(s"fold: ${balance.mkString(", ")}")
      }
      val (tunedDataset, selectedFeatures) = featureSelectionIncrementalCV(Utils.keepRows(dataset, fold.train.toArray), evalMetric)
      val classifier = classifierFactory()
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
      val f1 = evalMeasures("Overweight").F
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
    val finalClassifier = classifierFactory()

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
}

object ClassifierImpl {
  /** config file that fetches filepaths */
  val config = ConfigFactory.load()

  val logger = LoggerFactory.getLogger(this.getClass)


  def loadFollowees(accounts: Seq[TwitterAccount], variable: String): Map[String, Seq[String]] = {
    val followeeFile = scala.io.Source.fromFile(config.getString(s"classifiers.$variable.followeeRelations"))
    val handleToFollowees = (for (line <- followeeFile.getLines) yield {
      val handles = line.split("\t+")
      handles.head -> handles.tail.toSeq
    }).toMap
    followeeFile.close

    handleToFollowees
  }

  def loadFollowers(accounts: Seq[TwitterAccount]): Map[String, Seq[TwitterAccount]] = {
    val followerFile = scala.io.Source.fromFile(config.getString("classifiers.features.followerRelations"))
    val handleToFollowers: Map[String, Seq[String]] = (for (line <- followerFile.getLines) yield {
      val handles = line.split("\t")
      handles.head -> handles.tail.toSeq
    }).toMap
    followerFile.close

    val accountsFileStr = config.getString("classifiers.features.followerAccounts")
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
    weights: Map[String, Seq[(String, Double)]],
    falsePos: Seq[(String, Map[String, Seq[(String, Double)]])],
    falseNeg: Seq[(String, Map[String, Seq[(String, Double)]])]): Unit = {

    val numWeights = 30
    val numAccts = 20

    val barrier = "=" * 25
    // Initialize writer
    var writer = new BufferedWriter(new FileWriter(outputDir + "/weights.txt", false))
    weights.foreach { case (label, feats) =>
      writer.write(s"$label weights\n$barrier\n")
      writer.write(feats.take(numWeights).map(feat => s"${feat._1}\t${feat._2}").mkString("\n"))
      writer.write("\n")
    }
    writer.close()

    writer = new BufferedWriter(new FileWriter(outputDir + "/falsePositives.txt", false))
    writer.write(s"False positives\n$barrier\n\n")
    falsePos.foreach{ case (handle, acct) =>
      writer.write(s"$barrier\n$handle\n$barrier\n")
      acct.foreach{ case (label, feats) =>
        writer.write(s"$label weights\n$barrier\n")
        writer.write(feats.take(numWeights).map(feat => s"${feat._1}\t${feat._2}").mkString("\n"))
        writer.write("\n")
      }
      writer.write(s"$barrier\n$barrier\n\n")
    }
    writer.close()

    writer = new BufferedWriter(new FileWriter(outputDir + "/falseNegatives.txt", false))
    writer.write(s"False negatives\n$barrier\n\n")
    falseNeg.foreach{ case (handle, acct) =>
      writer.write(s"$barrier\n$handle\n$barrier\n")
      acct.foreach{ case (label, feats) =>
        writer.write(s"$label weights\n$barrier\n")
        writer.write(feats.take(numWeights).map(feat => s"${feat._1}\t${feat._2}").mkString("\n"))
        writer.write("\n")
      }
      writer.write(s"$barrier\n$barrier\n\n")
    }
    writer.close()
  }

  def outputAnalysis(outputFile: String, header: String, accounts: Seq[TwitterAccount], cls: ClassifierImpl, labels: Set[String]) {
    // Set progress bar
    var numAccountsToPrint = 20
    val numWeightsToPrint = 30
    val printedLabel = labels.toSeq.sorted.head
    val pb = new me.tongfei.progressbar.ProgressBar("outputAnalysis()", 100)
    pb.start()
    pb.maxHint(numAccountsToPrint)
    pb.setExtraMessage(header)

    // Initialize writer
    val writer = new BufferedWriter(new FileWriter(outputFile, false))
    var isFirst = true
    writer.write(header)

    // Iterate over accounts
    for (account <- accounts) {
      if (numAccountsToPrint > 0) {
        // Analyze account
        val (topWeights, dotProduct) = Utils.analyze(cls.subClassifier.get, labels, account, cls.featureExtractor)
        // Only print the general weights on the features once
        if (isFirst) {
          for ((label, sequence) <- topWeights) {
            writer.write(s"Top weights for $label:\n")
            var numToPrint = numWeightsToPrint
            for ((feature, score) <- sequence) {
              if ((numToPrint > 0) && (score > 0.0)) {
                writer.write(s"$feature -> $score\n")
                numToPrint = numToPrint - 1
              }
            }
            writer.write("================================\n")
          }
          isFirst = false
        }
        // Print hadamard product for every account
        writer.write(s"Hadamard product for ${account.handle}:\n")
        for ((label, sequence) <- dotProduct) {
          if (label == printedLabel) {
            var numToPrint = numWeightsToPrint
            for ((feature, score) <- sequence) {
              if ((numToPrint > 0) && (score > 0.0)) {
                writer.write(s"$feature -> $score\n")
                numToPrint = numToPrint - 1
              }
            }
          }
        }
        writer.write("================================\n")
      }
      pb.step()
      numAccountsToPrint -= 1
    }
    writer.close
    pb.stop()
  }

  def fMeasure(precision: Double, recall: Double, beta: Double): Double =
    (1 + Math.pow(beta, 2)) * ((precision * recall) / (Math.pow(beta, 2) * precision + recall))
}