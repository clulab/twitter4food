package org.clulab.twitter4food.util

import org.clulab.twitter4food.twitter4j._
import org.clulab.twitter4food.struct._
import com.typesafe.config.ConfigFactory

import scala.reflect.ClassTag
import org.clulab.learning.{Classifier, L1LinearSVMClassifier, LiblinearClassifier}
import org.clulab.struct.Lexicon

import scala.collection.mutable

object Utils {
  case class Config(
    useUnigrams: Boolean = false,
    useBigrams: Boolean = false,
    useTopics: Boolean = false,
    useDictionaries: Boolean = false,
    useAvgEmbeddings: Boolean = false,
    useMinEmbeddings: Boolean = false,
    useMaxEmbeddings: Boolean = false,
    useCosineSim: Boolean = false,
    useFollowers: Boolean = false,
    useFollowees: Boolean = false,
    useGender: Boolean = false,
    useRace: Boolean = false,
    useHuman: Boolean = false,
    datumScaling: Boolean = false,
    featureScaling: Boolean = false,
    fpnAnalysis: Boolean = false,
    runOnTest: Boolean = false,
    noTraining: Boolean = false,
    learningCurve: Boolean = false
  )

  def init(keyset: Int) = {
    (new TwitterAPI(keyset), ConfigFactory.load())
  }

  def loadHandles(fileName: String) = {
    scala.io.Source.fromFile(fileName).getLines.toList
      .foldLeft(Map[String, String]())(
        (map, line) => map + (line.split("\t")(0) -> line.split("\t")(1)))
  }

  def splitHandles[T: ClassTag](keyset: Int, numWindows: Int,
    collection: Map[T, String]): (Array[T], Array[String]) = {
    val window = collection.size/numWindows
    val lower = keyset*window
    val upper = if(keyset == numWindows-1) collection.size
    else (keyset+1)*window
    val subHandles = collection.keys.slice(lower, upper).toArray
    val subLabels = subHandles.map(k => collection(k))

    subHandles -> subLabels
  }

  def fetchAccounts(api: TwitterAPI, handles: Seq[String],
    fT: Boolean, fN: Boolean, appOnly: Boolean) = {
    val pb = new me.tongfei.progressbar.ProgressBar("fetchAccounts", 100)
    pb.start()

    pb.maxHint(handles.size)
    pb.setExtraMessage("Downloading ...")
    val accounts = handles.map(h => {
      val account = api.fetchAccount(h, fetchTweets = fT, fetchNetwork = fN,
        isAppOnly = appOnly)
      pb.step(); account
    })
    pb.stop()
    accounts
  }

  def parseArgs(args: Array[String]): Config = {
    val parser = new scopt.OptionParser[Config]("classifier") {
      head("classifier", "0.x")
      opt[Unit]('u', "unigrams") action { (x, c) =>
        c.copy(useUnigrams = true)} text "use unigrams"
      opt[Unit]('b', "bigrams") action { (x, c) =>
        c.copy(useBigrams = true)} text "use bigrams"
      opt[Unit]('t', "topics") action { (x, c) =>
        c.copy(useTopics = true)} text "use topics"
      opt[Unit]('d', "dictionaries") action { (x, c) =>
        c.copy(useDictionaries = true)} text "use dictionaries"
      opt[Unit]('a', "avgEmbeddings") action { (x, c) =>
        c.copy(useAvgEmbeddings = true)} text "use average embeddings"
      opt[Unit]('n', "minEmbeddings") action { (x, c) =>
        c.copy(useMinEmbeddings = true)} text "use minimum embeddings"
      opt[Unit]('x', "maxEmbeddings") action { (x, c) =>
        c.copy(useMaxEmbeddings = true)} text "use maximum embeddings"
      opt[Unit]('c', "cosineSim") action { (x, c) =>
        c.copy(useCosineSim = true)} text "use cosine similarity"
      opt[Unit]('g', "gender") action { (x, c) =>
        c.copy(useGender = true)} text "use gender classifier"
      opt[Unit]('r', "race") action { (x, c) =>
        c.copy(useRace = true)} text "use race classifier"
      opt[Unit]('h', "human") action { (x, c) =>
        c.copy(useHuman = true)} text "use human classifier"
      opt[Unit]('f', "followers") action { (x, c) =>
        c.copy(useFollowers = true)} text "use followers"
      opt[Unit]('F', "followees") action { (x, c) =>
        c.copy(useFollowees = true)} text "use followee handles"
      opt[Unit]('D', "datumScaling") action { (x, c) =>
        c.copy(datumScaling = true)} text "use datum scaling"
      opt[Unit]('S', "featureScaling") action { (x, c) =>
        c.copy(featureScaling = true)} text "use feature scaling"
      opt[Unit]("analysis") action { (x, c) =>
        c.copy(fpnAnalysis = true)} text "perform false positive/negative analysis"
      opt[Unit]("test") action { (x, c) =>
        c.copy(runOnTest = true)} text "run on test dataset (default: dev dataset)"
      opt[Unit]("noTraining") action { (x, c) =>
        c.copy(noTraining = true)} text "don't overwrite existing classifier if one exists"
      opt[Unit]("learningCurve") action { (x, c) =>
        c.copy(learningCurve = true)} text "analyze performance "
    }

    parser.parse(args, Config()).getOrElse(Config())
  }

  def analyze(c: LiblinearClassifier[String, String], labels: Set[String],
    test: TwitterAccount, fe: FeatureExtractor):
  (Map[String, Seq[(String, Double)]], Map[String, Seq[(String, Double)]]) = {

    val W = c.getWeights()
    val d = fe.mkDatum(test, "unknown")

    val topWeights = labels.foldLeft(Map[String, Seq[(String, Double)]]())(
      (map, l) => map + (l -> W.get(l).get.toSeq.sortWith(_._2 > _._2)))

    val dotProduct = labels.foldLeft(Map[String, Seq[(String, Double)]]())(
      (map, l) => {
        val weightMap = W.get(l).get.toSeq.toMap
        val feats = d.featuresCounter.toSeq
        map + (l -> feats.filter(f => weightMap.contains(f._1))
          .map(f => (f._1, f._2 * weightMap(f._1))).sortWith(_._2 > _._2))
      })

    (topWeights, dotProduct)
  }

  def analyze(filename: String, labels: Set[String], test: TwitterAccount,
    fe: FeatureExtractor):
  (Map[String, Seq[(String, Double)]], Map[String, Seq[(String, Double)]]) = {
    analyze(LiblinearClassifier.loadFrom[String, String](filename), labels,
      test, fe)
  }

  def prefix(f:String, sep:String):String = {
    var pref = f
    val i = f.indexOf(sep)
    if(i > 0) pref = f.substring(0, i)
    pref
  }

  def findFeatureGroups(sep:String, lexicon:Lexicon[String]):Map[String, Set[Int]] = {
    val groups = new mutable.HashMap[String, mutable.HashSet[Int]]()
    for(f <- lexicon.keySet) {
      val pref = prefix(f, sep)

      if(! groups.contains(pref))
        groups.put(pref, new mutable.HashSet[Int]())
      groups.get(pref).get += lexicon.get(f).get
    }

    val img = new mutable.HashMap[String, Set[Int]]()
    for(k <- groups.keySet) {
      img.put(k, groups.get(k).get.toSet)
    }
    img.toMap
  }

  def svmFactory():Classifier[String, String] = new L1LinearSVMClassifier[String, String]()

}