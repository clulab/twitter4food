package org.clulab.twitter4food.featureclassifier

import edu.arizona.sista.learning.{LinearSVMClassifier, RVFDataset}
import edu.arizona.sista.struct.Counter
import org.clulab.twitter4food.struct._
import org.clulab.twitter4food.util._
import java.io._

class GenderClassifier(
  val useUnigrams: Boolean = true,
  val useBigrams: Boolean = false,
  val useTopics: Boolean = false,
  val useDictionaries: Boolean = false,
  val useEmbeddings: Boolean = false) extends FeatureClassifier {


  val featureExtractor = new FeatureExtractor(useUnigrams, useBigrams, 
    useTopics, useDictionaries, useEmbeddings)
  var subClassifier = new LinearSVMClassifier[String, String]()
  var dataset = new RVFDataset[String, String]()

  override def train(accounts: Seq[TwitterAccount], labels: Seq[String]) = {
    // Clear current dataset if training on new one
    assert(accounts.size == labels.size)
    dataset = new RVFDataset[String, String]()
    // Populate dataset
 
    val pb = new me.tongfei.progressbar.ProgressBar("train", 100)
    pb.maxHint(accounts.size)
    pb.start()
    
    for (i <- accounts.indices) {
      dataset += featureExtractor.mkDatum(accounts(i), labels(i))
      pb.step()
    }

    pb.stop()
    subClassifier.train(dataset)
  }

  override def scoresOf(account: TwitterAccount): Counter[String] = {
    subClassifier.scoresOf(featureExtractor.mkDatum(account, "unknown"))
  }
}

object GenderClassifier {
  def main(args: Array[String]): Unit = {
    val params = TestUtils.parseArgs(args)
    val (api, config) = TestUtils.init(0, true)
    val gc = new GenderClassifier(params.useUnigrams, params.useBigrams,
      params.useTopics, params.useDictionaries, params.useEmbeddings)

    println("Loading training accounts...")
    val trainingData = FileUtils.load(config
      .getString("classifiers.gender.trainingData"))
    println("Loading dev accounts...")
    val devData = FileUtils.load(config
      .getString("classifiers.gender.devData"))
    println("Loading test accounts...")
    val testData = FileUtils.load(config
      .getString("classifiers.gender.testData"))

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
      new File(config.getString("classifier") + "/gender/results" + 
      fileExt + ".txt")))

    val gridCbyK = Array.ofDim[Double](7,7)

    val runTest = (trainingSet: Seq[TwitterAccount], 
      trainingLabels: Seq[String], 
      testSet: Seq[TwitterAccount],
      testingLabels: Seq[String],
      _C: Double,
      K: Int) => {
      
      println(s"Training with C=${_C} and top-${K} tweets")

      gc.subClassifier = new LinearSVMClassifier[String, String](C=_C)
      val customAccounts = trainingSet.map(t => {
          val numTweets = Math.min(K, t.tweets.size)
          new TwitterAccount(t.handle, t.id, t.name, t.lang, t.url, 
            t.location, t.description, t.tweets.slice(0, numTweets))
        })

      // Train with top K tweets
      gc.train(customAccounts, trainLabels)

      val pb = new me.tongfei.progressbar.ProgressBar("testing", 100)
      pb.maxHint(testSet.size)
      pb.start()
      val predictedLabels = testSet.map(u => { pb.step(); gc.classify(u); })
      pb.stop()

      val (evalMeasures, microAvg, macroAvg) = Eval.evaluate(testingLabels, 
        predictedLabels)
        
      val df = new java.text.DecimalFormat("#.###")

      println(s"C=${_C}, #K=$K")
      println(evalMeasures.mkString("\n"))
      println(s"Macro avg F-1 : ${df.format(macroAvg)}")
      println(s"Micro avg F-1 : ${df.format(microAvg)}")
      writer.write(s"C=${_C}, #K=$K")
      writer.write(evalMeasures.mkString("\n"))
      writer.write(s"Macro avg F-1 : ${df.format(macroAvg)}\n")
      writer.write(s"Micro avg F-1 : ${df.format(microAvg)}\n")
      writer.flush()

      microAvg
    }

    for(i <- cFolds.indices) {
      val _C = cFolds(i)       
      for(j <- tweetFolds.indices) {
        val K = tweetFolds(j) 
        gridCbyK(i)(j) = runTest(trainUsers, trainLabels, 
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

    // Final run on test Set
    runTest(trainUsers ++ devUsers, trainLabels ++ devLabels, testUsers,
      testLabels, cFolds(iMax), tweetFolds(jMax))

    println("*****Test complete*****")
    writer.write("*****Test complete*****\n")
    writer.flush()

    writer.close()
  }
}