package org.clulab.twitter4food.featureclassifier

import edu.arizona.sista.learning._
import edu.arizona.sista.struct.{Counter, Lexicon}
import org.clulab.twitter4food.struct._
import org.clulab.twitter4food.util._
import java.io.{BufferedWriter, FileWriter}
import com.typesafe.config.ConfigFactory
import scala.collection.JavaConverters._

class ClassifierImpl(
  val useUnigrams: Boolean = true,
  val useBigrams: Boolean = false,
  val useTopics: Boolean = false,
  val useDictionaries: Boolean = false,
  val useEmbeddings: Boolean = false) extends FeatureClassifier {


  val featureExtractor = new FeatureExtractor(useUnigrams, useBigrams, 
    useTopics, useDictionaries, useEmbeddings)
  var subClassifier: Option[LiblinearClassifier[String, String]] = None
  var dataset = new RVFDataset[String, String]()
  val config = ConfigFactory.load()

  def addDatum(account: TwitterAccount, label: String) = {
    dataset += featureExtractor.mkDatum(account, label)
  }

  def loadLexicons(lexiconMap: Map[String, Seq[String]]) = {
    val l = lexiconMap map { 
      case (k, v) => (k, v.map(Lexicon.loadFrom[String](_))) 
    }
    featureExtractor.lexicons = Some(l)
  }

  override def train(accounts: Seq[TwitterAccount], labels: Seq[String]) = {
    assert(accounts.size == labels.size)
    // Clear current dataset if training on new one
    dataset = new RVFDataset[String, String]()
 
    val pb = new me.tongfei.progressbar.ProgressBar("train()", 100)
    pb.start()
    pb.maxHint(accounts.size)
    pb.setExtraMessage("Training...")
    
    // Populate dataset
    for (i <- accounts.indices) {
      addDatum(accounts(i), labels(i))
      pb.step()
    }

    pb.stop()
    subClassifier.get.train(dataset)
  }

  override def scoresOf(account: TwitterAccount): Counter[String] = {
    if(subClassifier.isDefined) {
      subClassifier.get.scoresOf(featureExtractor.mkDatum(account, "unknown"))
      } else throw new RuntimeException("ERROR: must train before using scoresOf!")
  }

  def _runTest(trainingSet: Seq[TwitterAccount], 
      trainingLabels: Seq[String], 
      testSet: Seq[TwitterAccount],
      _C: Double,
      K: Int,
      ctype: String,
      args: Array[String]) = {
    
    subClassifier = Some(new LinearSVMClassifier[String, String](C=_C))
    val labelSet = trainingLabels.toSet
    val lexMap = labelSet.foldLeft(Map[String, Seq[String]]())(
      (m, l) => m + (l -> 
        config.getStringList(s"classifiers.$ctype.$l.lexicons").asScala.toList))

    if(useDictionaries) loadLexicons(lexMap)
    val customAccounts = trainingSet.map(t => {
      val numTweets = Math.min(K, t.tweets.size)
      new TwitterAccount(t.handle, t.id, t.name, t.lang, t.url, 
        t.location, t.description, t.tweets.slice(0, numTweets))
      })

    val opt = config.getString(s"classifiers.$ctype.model")
    val fout = s"${opt}/svm_${args.mkString("")}_${_C}_${K}.dat"

    // Train with top K tweets
    train(customAccounts, trainingLabels)
    subClassifier.get.saveTo(fout)

    val pb = new me.tongfei.progressbar.ProgressBar("runTest()", 100)
    pb.start()
    pb.maxHint(testSet.size.toInt)
    pb.setExtraMessage("Predicting...")

    val predictedLabels = testSet.map(u => { pb.step(); classify(u); })
    pb.stop()

    predictedLabels
  }

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
    writer.write(s"C=${_C}, #K=${K}\n")
    writer.write(evalMeasures.mkString("\n"))
    evalMeasures.keys.foreach(l => {
      writer.write(l + "\n" + "FP:\n")
      writer.write(s"${evalMeasures(l).FPAccounts.map(u => u.handle).mkString("\n")}\nFN:\n")
      writer.write(s"${evalMeasures(l).FNAccounts.map(u => u.handle).mkString("\n")}\n")
        })
    writer.write(s"\nMacro avg F-1 : ${df.format(macroAvg)}\n")
    writer.write(s"Micro avg F-1 : ${df.format(microAvg)}\n")
    writer.flush()

    (evalMeasures, microAvg, macroAvg)

  }

  def runTest(args: Array[String], ctype: String) = {

    println("Loading training accounts...")
    val trainingData = FileUtils.load(config
      .getString(s"classifiers.$ctype.trainingData"))
    println("Loading dev accounts...")
    val devData = FileUtils.load(config
      .getString(s"classifiers.$ctype.devData"))
    println("Loading test accounts...")
    val testData = FileUtils.load(config
      .getString(s"classifiers.$ctype.testData"))

    val fileExt = args.mkString("")
    val tweetFolds = Array(10, 50, 100, 500, 1000, 2000, 5000)
    val cFolds = Array(0.001, 0.01, 0.1, 1, 10, 100, 1000)
    
    /*
     * (i)  Tune parameters 
     * (ii) Pick top-K tweets
     */

    val (trainUsers, devUsers, testUsers) = (trainingData.keys.toArray, 
      devData.keys.toArray, testData.keys.toArray)

    val (trainLabels, devLabels, testLabels) = (trainingData.values.toArray, 
      devData.values.toArray, testData.values.toArray)

    val writer = new BufferedWriter(new FileWriter(
      config.getString("classifier") + s"/$ctype/opt" + 
      fileExt + ".txt",true))

    val gridCbyK = Array.ofDim[Double](7,7)
    
    val unitTest = (trainingSet: Seq[TwitterAccount], 
      trainingLabels: Seq[String], 
      testSet: Seq[TwitterAccount],
      testingLabels: Seq[String],
      _C: Double,
      K: Int) => {
      
      println(s"Training with C=${_C} and top-${K} tweets")

      val predictedLabels = _runTest(trainingSet, trainingLabels,
        testSet, _C, K, ctype, args)
      
      val (evalMeasures, microAvg, macroAvg) = _evaluate(testingLabels, 
        predictedLabels, testSet, writer, _C, K)
      
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
          iMax = i; jMax = j
        }

    println(s"Best C = ${cFolds(iMax)}, Top K = ${tweetFolds(jMax)}")
    writer.write(s"Best C = ${cFolds(iMax)}, Top K = ${tweetFolds(jMax)}\n")
    println("Testing with test users")

    writer.write("*****Test*****\n")
    writer.flush()

    // Final run on test Set
    unitTest(trainUsers ++ devUsers, trainLabels ++ devLabels, 
      testUsers, testLabels, cFolds(iMax), tweetFolds(jMax))

    println("*****Test complete*****")
    writer.write("*****Test complete*****\n")
    writer.flush()

    writer.close()
  }

  def predict(args: Array[String], ctype: String, testFile: String,
    _C: Double, K: Int) = {
    val allTrainData = FileUtils.load(config.getString(
      s"classifiers.$ctype.allTrainData"))

    val allTestData = FileUtils.load(testFile)

    val allTrainAccounts = allTrainData.map(_._1).toArray
    val allTrainLabels = allTrainData.map(_._2).toArray

    val testAccounts = allTestData.map(_._1).toArray

    val predictedLabels = _runTest(allTrainAccounts, allTrainLabels,
        testAccounts, _C, K, ctype, args)

    val writer = new BufferedWriter(new FileWriter(
      config.getString(s"classifiers.$ctype.predictions")))

    for(i <- testAccounts.indices) {
      writer.write(s"${testAccounts(i).handle}\t${predictedLabels(i)}\n")
      writer.flush()
      }
    writer.close()
  }
}