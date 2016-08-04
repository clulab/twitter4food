package org.clulab.twitter4food.featureclassifier

import org.clulab.learning._
import org.clulab.struct.{Counter, Lexicon}
import org.clulab.twitter4food.struct._
import org.clulab.twitter4food.util._
import java.io.{BufferedWriter, FileWriter, PrintWriter}

import com.typesafe.config.ConfigFactory
import org.slf4j.LoggerFactory

import scala.collection.JavaConverters._

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
  * @date 04-02-2016
  */

class ClassifierImpl(
  val useUnigrams: Boolean,
  val useBigrams: Boolean,
  val useTopics: Boolean,
  val useDictionaries: Boolean,
  val useEmbeddings: Boolean,
  val useCosineSim: Boolean,
  val useFollowers: Boolean,
  val useFollowees: Boolean,
  val useGender: Boolean,
  val useRace: Boolean,
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
    useTopics=useTopics,
    useDictionaries=useDictionaries,
    useEmbeddings=useEmbeddings,
    useCosineSim=useCosineSim,
    useFollowers=useFollowers,
    useFollowees=useFollowees,
    useGender=useGender,
    useRace=useRace,
    datumScaling=datumScaling,
    customFeatures=customFeatures)

  /** subClassifier that does the actual training over {@link dataset} */
  var subClassifier: Option[LiblinearClassifier[String, String]] = None

  /** config file that fetches filepaths */
  val config = ConfigFactory.load()

  val logger = LoggerFactory.getLogger(this.getClass)

  var scaleRange: Option[ScaleRange[String]] = None
  val lowerBound = 0.0
  val upperBound = 1.0

  /** Populates list of lexicons from config file. Separate function
    * for easy testing.
    *
    * @param labelSet Set of labels
    * @param ctype Type of classifier
    * @return map of label -> Seq of lexicon file names
    */
  def populateLexiconList(labelSet: Set[String], ctype: String) = {
    labelSet.foldLeft(Map[String, Seq[String]]())(
      (m, l) => m + (l ->
        config.getStringList(s"classifiers.$ctype.$l.lexicons").asScala.toList))
  }

  def constructDataset(
    accounts: Seq[TwitterAccount],
    followers: Option[Map[String, Seq[TwitterAccount]]],
    labels: Seq[String]): RVFDataset[String, String] = {

    // Load lexicons before calling train
    if(useDictionaries) {
      // For each label, populate list of lexicon filepaths from config
      val lexMap = populateLexiconList(labels.toSet, this.variable)
      this.featureExtractor.setLexicons(lexMap)
    }

    if (useFollowers && followers.nonEmpty) {
      this.featureExtractor.setFollowers(followers.get)
    }

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
        (account.handle, featureExtractor.mkDatum(account, label))
      }
    }).seq.sortBy(_._1).unzip._2

    pb.stop()

    datums.foreach(datum => this.synchronized { dataset += datum })

    dataset
  }

  /**
    * Sequentially adds a [[RVFDatum]] of (label, mkDatum(account)), first loading followers if necessary
    * @param accounts
    * @param labels
    */
  def train(accounts: Seq[TwitterAccount], labels: Seq[String]) = {
    val followers = if (useFollowers) Option(loadFollowers(accounts)) else None
    train(accounts, followers, labels)
  }

  /** Sequentially adds a [[RVFDatum]] of (label, mkDatum(account))
    *
    * @param accounts: Sequence of training accounts
    * @param labels: Sequence of annotated labels for each account
    * @return Unit
    */
  def train(accounts: Seq[TwitterAccount], followers: Option[Map[String, Seq[TwitterAccount]]], labels: Seq[String]) = {
    assert(accounts.size == labels.size)

    // Clear current dataset if training on new one
    val dataset = constructDataset(accounts, followers, labels)

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
    val fout = s"$opt/svm_${args.mkString("").replace("-", "").sorted}_${_C}_$K.dat"

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
    val trainingData = FileUtils.load(config
      .getString(s"classifiers.$ctype.trainingData"))
    println("Loading dev accounts...")
    val devData = FileUtils.load(config
      .getString(s"classifiers.$ctype.devData"))
    println("Loading test accounts...")
    val testData = if(!devOnly) Some(FileUtils.load(config.getString(s"classifiers.$ctype.testData"))) else None

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

    /** For a given classifier, load its associated train, dev, and test
      * accounts, and write results to file.
      *
      * @param trainingSet
      * @param trainingLabels
      * @param testingSet
      * @param testingLabels
      * @param _C hyperparameter for subClassifier 
      * @param K threshold for top-K tweets for each user
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
    val allTrainData = FileUtils.load(config.getString(
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

    val allTestData = FileUtils.load(testFile)
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

  def featureSelectionIncremental(accounts: Map[TwitterAccount, String], followers: Map[String, Seq[TwitterAccount]]) {
    val dataset = constructDataset(accounts.keys.toSeq, Option(followers), accounts.values.toSeq)
    val featureGroups = Utils.findFeatureGroups(":", dataset.featureLexicon)
    logger.debug(s"Found ${featureGroups.size} feature groups:")
    for(f <- featureGroups.keySet) {
      logger.debug(s"Group $f containing ${featureGroups.get(f).get.size} features.")
    }
    val chosenGroups = Datasets.incrementalFeatureSelection[String, String](
      dataset, Utils.svmFactory, Eval.f1ForLabel("Overweight"), featureGroups)
    logger.info(s"Selected ${chosenGroups.size} feature groups: " + chosenGroups)
  }
}

object ClassifierImpl {
  /** config file that fetches filepaths */
  val config = ConfigFactory.load()

  val logger = LoggerFactory.getLogger(this.getClass)

  def loadFollowers(accounts: Seq[TwitterAccount]): Map[String, Seq[TwitterAccount]] = {
    val followerFile = scala.io.Source.fromFile(config.getString("classifiers.features.followerRelations"))
    val handleToFollowers: Map[String, Seq[String]] = (for (line <- followerFile.getLines) yield {
      val handles = line.split("\t")
      handles.head -> handles.tail.toSeq
    }).toMap
    followerFile.close

    val accountsFileStr = config.getString("classifiers.features.followerAccounts")
    val followerAccounts = FileUtils.load(accountsFileStr)
    val handleToFollowerAccts = accounts.map{ account =>
      val followerAccount = handleToFollowers.getOrElse(account.handle, Nil).flatMap { followerHandle =>
        followerAccounts.find{ case(f, label) => f.handle == followerHandle }
      }.map(_._1)
      account.handle -> followerAccount
    }.toMap

    handleToFollowerAccts
  }
}