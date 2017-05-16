package org.clulab.twitter4food.featureclassifier

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.{Files, Paths}

import com.typesafe.config.ConfigFactory
import org.clulab.learning.{L1LinearSVMClassifier, LiblinearClassifier}
import org.clulab.struct.Counter
import org.clulab.twitter4food.util._
import org.clulab.twitter4food.util.Utils._
import org.clulab.twitter4food.struct.TwitterAccount
import org.slf4j.LoggerFactory

/**
  * Classifier to predict if a given twitter account represents an organization
  * or an individual. Implements a customFeatures method to parse the account
  * description and count #(words) that fall in person/organization Synset
  * @author adikou
  */

class HumanClassifier(
  useUnigrams: Boolean = false,
  useBigrams: Boolean = false,
  useName: Boolean = false,
  useTopics: Boolean = false,
  useDictionaries: Boolean = false,
  useAvgEmbeddings: Boolean = false,
  useMinEmbeddings: Boolean = false,
  useMaxEmbeddings: Boolean = false,
  useCosineSim: Boolean = false,
  useTimeDate: Boolean = false,
  useFollowers: Boolean = false,
  useFollowees: Boolean = false,
  useRT: Boolean = false,
  datumScaling: Boolean = false,
  featureScaling: Boolean = false,
  customFeatures: TwitterAccount => Counter[String])
  extends ClassifierImpl(
    useUnigrams=useUnigrams,
    useBigrams=useBigrams,
    useName=useName,
    useTopics=useTopics,
    useDictionaries=useDictionaries,
    useAvgEmbeddings=useAvgEmbeddings,
    useMinEmbeddings=useMinEmbeddings,
    useMaxEmbeddings=useMaxEmbeddings,
    useCosineSim=useCosineSim,
    useLocation=false,
    useTimeDate=useTimeDate,
    useFoodPerc=false,
    useCaptions=false,
    useFollowers=useFollowers,
    useFollowees=useFollowees,
    useRT=useRT,
    useGender=false,
    useAge=false,
    useRace=false,
    useHuman=false,
    dictOnly=false,
    denoise=false,
    datumScaling=datumScaling,
    featureScaling=featureScaling,
    variable = "human",
    customFeatures=customFeatures
  ) {
  val labels = Set("human", "org")
}

object HumanClassifier {
  import ClassifierImpl._

  /**
    * An empty counter for when custom features are not desired.
    */
  def nullFeatures(account: TwitterAccount) = new Counter[String]()

  /** Add a custom feature counter for the account based on description
    * @param account Twitter account
    * @return counter custom Counter[String] that keeps a count of "wn_human"
    *         and "wn_org" based on #(words) in person/organization Synset
    */
  def customFeatures(account: TwitterAccount): Counter[String] = {
    val SINGULAR_PRONOUNS = Set("I", "me", "you", "she", "her", "he",
      "him", "it", "myself", "yourself", "itself",
      "himself", "herself", "self", "oneself")
    val PLURAL_PRONOUNS = Set("we", "us", "they", "them", "ourselves",
      "yourselves", "themselves")

    def isSingularPronoun(word: String) = SINGULAR_PRONOUNS.contains(word)
    def isPluralPronoun(word: String) = PLURAL_PRONOUNS.contains(word)

    val PERSON_CLASS = Set("person", "individual", "mortal", "self", "someone",
      "somebody", "soul")
    val ORG_CLASS = Set("organisation", "organization", "establishment",
      "governance", "governing body", "administration",
      "arrangement", "constitution", "formation",
      "institution", "building", "edifice", "structure")

    def intersection(A: Set[String], B: Set[String]) = A.intersect(B)
    def isPersonClass(set: Set[String]) = intersection(set, PERSON_CLASS).nonEmpty
    def isOrgClass(set: Set[String]) = intersection(set, ORG_CLASS).nonEmpty

    /** Recurse each synset until synset becomes too generic, or one or more
      * synset reaches person/org synset.
      * @param word Description word
      * @return label "human", "org" or ""
      */
    def getSubFeatureType(word: String): String = {
      val hyp = new HypernymSet()
      var level = 0
      var hSet = Set(word)
      var hSubset = hyp.subHypernym(hSet)

      while(level < 3 && !(isPersonClass(hSubset) || isOrgClass(hSubset))) {
        hSet = hSubset
        hSubset = hyp.subHypernym(hSet)
        level += 1
      }

      val A = isPersonClass(hSubset)
      val B = isOrgClass(hSubset)

      if(A || B) {
        if(A && B) {
          val s1 = intersection(hSubset, PERSON_CLASS)
          val s2 = intersection(hSubset, ORG_CLASS)

          if(s1.size > s2.size) "humans"
          else if(s2.size > s1.size) "org"
          else if(scala.util.Random.nextInt(2) == 0) "human" else "org"
        }
        else if(A) "human"
        else "org"
      }
      else ""
    }

    val counter = new Counter[String]()

    // Description features
    val dTags = Tokenizer.annotate(account.description)
      .filter(tt => "NO".contains(tt.tag))

    dTags.foreach{
      case singular if singular.tag == "O" && isSingularPronoun(singular.token) =>
        counter.incrementCount("hcDescriptionSingular")
      case plural if plural.tag == "O" && isPluralPronoun(plural.token) =>
        counter.incrementCount("hcDescriptionPlural")
      case humanWord if humanWord.tag == "N" && getSubFeatureType(humanWord.token) == "human" =>
        counter.incrementCount("hcDescriptionHuman")
      case orgWord if orgWord.tag == "N" && getSubFeatureType(orgWord.token) == "org" =>
        counter.incrementCount("hcDescriptionOrg")
      case _ => ()
    }

    // Tweet features
    val tTags = account.tweets.flatMap(tweet => Tokenizer.annotate(tweet.text))
      .filter(tt => "NO".contains(tt.tag))

    tTags.foreach{
      case singular if singular.tag == "O" && isSingularPronoun(singular.token) =>
        counter.incrementCount("hcTweetSingular")
      case plural if plural.tag == "O" && isPluralPronoun(plural.token) =>
        counter.incrementCount("hcTweetPlural")
      case humanWord if humanWord.tag == "N" && getSubFeatureType(humanWord.token) == "human" =>
        counter.incrementCount("hcTweetHuman")
      case orgWord if orgWord.tag == "N" && getSubFeatureType(orgWord.token) == "org" =>
        counter.incrementCount("hcTweetOrg")
      case _ => ()
    }

    // Maybe it's the proportion that matters
    val ds = counter.getCount("hcDescriptionSingular")
    val dp = counter.getCount("hcDescriptionPlural")
    if (ds != 0 || dp != 0) counter.setCount("hcDescriptionSingularProp", ds / (ds + dp))

    val dhuman = counter.getCount("hcDescriptionHuman")
    val dorg = counter.getCount("hcDescriptionOrg")
    if (dhuman != 0 || dorg != 0) counter.setCount("hcDescriptionHumanProp", dhuman / (dhuman + dorg))

    val ts = counter.getCount("hcTweetSingular")
    val tp = counter.getCount("hcTweetPlural")
    if (ts != 0 || tp != 0) counter.setCount("hcTweetSingularProp", ts / (ts + tp))

    val thuman = counter.getCount("hcTweetHuman")
    val torg = counter.getCount("hcTweetOrg")
    if (thuman != 0 || torg != 0) counter.setCount("hcDescriptionHumanProp", thuman / (thuman + torg))

    prepend("hcCustom:", counter)
  }

  def main(args: Array[String]) = {
    val params = Utils.parseArgs(args)
    val config = ConfigFactory.load
    val logger = LoggerFactory.getLogger(this.getClass)

    val portions = if (params.learningCurve) (1 to 20).map(_.toDouble / 20) else Seq(1.0)

    val nonFeatures = Seq("--analysis", "--test", "--noTraining", "--learningCurve")
    // This model and results are specified by all input args that represent featuresets
    val fileExt = args.filterNot(nonFeatures.contains).sorted.mkString("").replace("-", "")

    val outputDir = config.getString("classifier") + "/human/results/" + fileExt
    if (!Files.exists(Paths.get(outputDir))) {
      if (new File(outputDir).mkdir()) logger.info(s"Created output directory $outputDir")
      else logger.info(s"ERROR: failed to create output directory $outputDir")
    }

    val toTrainOn = if (params.runOnTest) {
      logger.info("Loading training accounts...")
      val trainData = FileUtils.loadTwitterAccounts(config.getString("classifiers.human.trainingData")).toSeq
      logger.info("Loading dev accounts...")
      val devData = FileUtils.loadTwitterAccounts(config.getString("classifiers.human.devData")).toSeq
      trainData ++ devData
    } else {
      logger.info("Loading training accounts...")
      FileUtils.loadTwitterAccounts(config.getString("classifiers.human.trainingData")).toSeq
    }

    val followers = if(params.useFollowers) Option(ClassifierImpl.loadFollowers(toTrainOn.map(_._1))) else None
    val followees = if(params.useFollowees) Option(ClassifierImpl.loadFollowees(toTrainOn.map(_._1), "human")) else None

    val modelDir = s"${config.getString("human")}/model"
    if (!Files.exists(Paths.get(modelDir))) {
      if (new File(modelDir).mkdir()) logger.info(s"Created output directory $modelDir")
      else logger.error(s"ERROR: failed to create output directory $modelDir")
    }
    val modelFile = s"${config.getString("human")}/model/$fileExt.dat"

    val customAction = (twitterAccount: TwitterAccount) =>
      if (params.useCustomAction) HumanClassifier.customFeatures(twitterAccount) else new Counter[String]()

    val classifiers = for {
      portion <- portions
      maxIndex = (portion * toTrainOn.length).toInt
    } yield {
      val (trainAccounts, trainLabels) = toTrainOn.slice(0, maxIndex).unzip

      val hc = new HumanClassifier(
        useUnigrams = params.useUnigrams,
        useBigrams = params.useBigrams,
        useName = params.useName,
        useTopics = params.useTopics,
        useDictionaries = params.useDictionaries,
        useAvgEmbeddings = params.useAvgEmbeddings,
        useMinEmbeddings = params.useMinEmbeddings,
        useMaxEmbeddings = params.useMaxEmbeddings,
        useCosineSim = params.useCosineSim,
        useTimeDate = params.useTimeDate,
        useFollowers = params.useFollowers,
        useFollowees = params.useFollowees,
        useRT = params.useRT,
        datumScaling = params.datumScaling,
        featureScaling = params.featureScaling,
        customAction
      )

      logger.info("Training classifier...")
      hc.setClassifier(new L1LinearSVMClassifier[String, String]())
      hc.train(trainAccounts, trainLabels, followers, followees)
      // Only save models using full training
      if (maxIndex == toTrainOn.length) hc.subClassifier.get.saveTo(modelFile)

      (portion, maxIndex, hc)
    }
    val toTestOn = if (params.runOnTest) {
      logger.info("Loading test accounts...")
      FileUtils.loadTwitterAccounts(config.getString("classifiers.human.testData"))
    } else {
      logger.info("Loading dev accounts...")
      FileUtils.loadTwitterAccounts(config.getString("classifiers.human.devData"))
    }

    val evals = for ((portion, numAccounts, hc) <- classifiers) yield {

      // Set progress bar
      val pb = new me.tongfei.progressbar.ProgressBar("main()", 100)
      pb.start()
      pb.maxHint(toTestOn.size)
      pb.setExtraMessage("Testing on dev accounts...")

      // Classify accounts
      val testSetLabels = toTestOn.values.toSeq
      val predictedLabels = toTestOn.keys.toSeq.map { u =>
        pb.step()
        hc.classify(u)
      }

      pb.stop()

      // Print results
      val (evalMeasures, microAvg, macroAvg) = Eval.evaluate(testSetLabels, predictedLabels, toTestOn.keys.toSeq)

      val evalMetric = evalMeasures(hc.labels.toSeq.sorted.head)
      val precision = evalMetric.P
      val recall = evalMetric.R

      if (portion == 1.0) {
        if (params.fpnAnalysis & hc.subClassifier.nonEmpty &
          (evalMetric.FNAccounts.nonEmpty || evalMetric.FPAccounts.nonEmpty)) {
          // Perform analysis on false negatives and false positives
          println("False negatives:")
          evalMetric.FNAccounts.foreach(account => print(account.handle + "\t"))
          println("\n====")
          outputAnalysis(outputDir + "/analysisFN.txt", "*** False negatives ***\n\n", evalMetric.FNAccounts, hc, hc.labels)

          println("False positives:")
          evalMetric.FPAccounts.foreach(account => print(account.handle + "\t"))
          println("\n====")
          outputAnalysis(outputDir + "/analysisFP.txt", "*** False positives ***\n\n", evalMetric.FPAccounts, hc, hc.labels)
        }

        // Save results
        val writer = new BufferedWriter(new FileWriter(outputDir + "/analysisMetrics.txt", false))
        writer.write(s"Precision: $precision\n")
        writer.write(s"Recall: $recall\n")
        writer.write(s"F-measure (harmonic mean): ${fMeasure(precision, recall, 1)}\n")
        writer.write(s"F-measure (recall 5x): ${fMeasure(precision, recall, .2)}\n")
        writer.write(s"Macro average: $macroAvg\n")
        writer.write(s"Micro average: $microAvg\n")
        writer.close()

        // Save individual predictions for bootstrap significance
        val predicted = new BufferedWriter(new FileWriter(outputDir + "/predicted.txt", false))
        predicted.write(s"gold\tpred\n")
        testSetLabels.zip(predictedLabels).foreach(acct => predicted.write(s"${acct._1}\t${acct._2}\n"))
        predicted.close()
      }

      (portion, numAccounts, precision, recall, macroAvg, microAvg)
    }

    println(s"\n$fileExt\n%train\t#accts\tp\tr\tf1\tf1(r*5)\tmacro\tmicro")
    evals.foreach { case (portion, numAccounts, precision, recall, macroAvg, microAvg) =>
      println(s"$portion\t$numAccounts\t$precision\t$recall\t${fMeasure(precision, recall, 1)}\t${fMeasure(precision, recall, .2)}" +
        s"\t$macroAvg\t$microAvg")
    }


  }
}