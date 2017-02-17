package org.clulab.twitter4food.util

import org.clulab.twitter4food.twitter4j._
import org.clulab.twitter4food.struct._
import com.typesafe.config.ConfigFactory
import org.slf4j.LoggerFactory

import scala.reflect.ClassTag
import org.clulab.learning._
import org.clulab.struct.{Counter, Lexicon}

import scala.collection.mutable
import scala.util.Random

object Utils {
  case class Config(
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
    useCustomAction: Boolean = false,
    useFollowers: Boolean = false,
    useFollowees: Boolean = false,
    useRT: Boolean = false,
    useGender: Boolean = false,
    useAge: Boolean = false,
    useRace: Boolean = false,
    useHuman: Boolean = false,
    datumScaling: Boolean = false,
    featureScaling: Boolean = false,
    fpnAnalysis: Boolean = false,
    runOnTest: Boolean = false,
    learningCurve: Boolean = false
  )

  val logger = LoggerFactory.getLogger(this.getClass)

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
      opt[Unit]('i', "name") action { (x, c) =>
        c.copy(useName = true)} text "use n-grams generated from name and handle"
      opt[Unit]('t', "topics") action { (x, c) =>
        c.copy(useTopics = true)} text "use LDA topics"
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
      opt[Unit]('w', "timeDate") action { (x, c) =>
        c.copy(useTimeDate = true)} text "use tweet time and date"
      opt[Unit]('s', "customAction") action { (x, c) =>
        c.copy(useCustomAction = true)} text "use any custom actions for the classifier"
      opt[Unit]('g', "gender") action { (x, c) =>
        c.copy(useGender = true)} text "use gender classifier"
      opt[Unit]('o', "age") action { (x, c) =>
        c.copy(useAge = true)} text "use age classifier"
      //opt[Unit]('r', "race") action { (x, c) =>
      //  c.copy(useRace = true)} text "use race classifier (not implemented)"
      opt[Unit]('h', "human") action { (x, c) =>
        c.copy(useHuman = true)} text "use human classifier"
      opt[Unit]('f', "followers") action { (x, c) =>
        c.copy(useFollowers = true)} text "use followers' features (same as this user)"
      opt[Unit]('F', "followees") action { (x, c) =>
        c.copy(useFollowees = true)} text "use followee handles"
      opt[Unit]('r', "retweet") action { (x, c) =>
        c.copy(useRT = true)} text "treat retweet n-grams differently"
      opt[Unit]('D', "datumScaling") action { (x, c) =>
        c.copy(datumScaling = true)} text "use datum scaling"
      opt[Unit]('S', "featureScaling") action { (x, c) =>
        c.copy(featureScaling = true)} text "use feature scaling"
      opt[Unit]("analysis") action { (x, c) =>
        c.copy(fpnAnalysis = true)} text "perform false positive/negative analysis"
      opt[Unit]("test") action { (x, c) =>
        c.copy(runOnTest = true)} text "run on test dataset (default: dev dataset)"
      opt[Unit]("learningCurve") action { (x, c) =>
        c.copy(learningCurve = true)} text "analyze performance "
    }

    val opts = parser.parse(args, Config())

    if(opts.isEmpty) throw new IllegalArgumentException(s"args ${args.mkString(" ")} are not supported!")

    opts.get
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

  def analyze(w: Map[String, Counter[String]], d:Datum[String, String]): Map[String, Seq[(String, Double)]] = {
    w.keys.foldLeft(Map[String, Seq[(String, Double)]]())(
      (map, l) => {
        val weightMap = w(l).toSeq.toMap
        val feats = d.featuresCounter.toSeq
        map + (l -> feats.filter(f => weightMap.contains(f._1))
          .map(f => (f._1, f._2 * weightMap(f._1))).sortWith(_._2 > _._2))
      })
  }

  def analyze(filename: String, labels: Set[String], d: Datum[String, String]):
  Map[String, Seq[(String, Double)]] = {
    analyze(LiblinearClassifier.loadFrom[String, String](filename).getWeights(), d)
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

  def svmFactory(): LiblinearClassifier[String, String] = new L1LinearSVMClassifier[String, String]()

  // Helper function for mapping a prefix onto all labels in a counter
  def prepend (prefix: String, counter: Counter[String]): Counter[String] = {
    val temp = new Counter[String]()
    for ((label, score) <- counter.toSeq)
      temp.setCount(prefix + label, score)
    temp
  }

  /**
    * Reduce a Seq of [[TwitterAccount]]s to the largest size possible to satisfy the proportions of labels designated
    */
  def subsample(accounts: (Seq[(TwitterAccount, String)]),
    desiredProps: Map[String, Double],
    seed: Int = 773): Seq[(TwitterAccount, String)] = {
    assert(accounts.map(_._2).toSet == desiredProps.keySet)
    if (desiredProps.values.sum != 1.0) logger.warn("Desired proportions do not sum to 1!")

    val r = new Random(seed)
    val byClass = accounts.groupBy(_._2)
    val currentDims = byClass.map{ case (lbl, accts) => lbl -> accts.length }
    val currentProps = currentDims.mapValues(_ / accounts.length.toDouble)

    val limiting = currentProps.map{ case (lbl, currProp) => lbl -> currProp / desiredProps(lbl) }.minBy(_._2)._1
    val newTotal = currentDims(limiting) / desiredProps(limiting)
    val desiredDims = desiredProps.map{ case (lbl, dprop) => lbl -> (dprop * newTotal toInt) }

    logger.debug(s"Old dimensions: ${currentDims.map(pair => s"${pair._1} -> ${pair._2}").mkString(", ")}")
    logger.debug(s"New dimensions: ${desiredDims.map(pair => s"${pair._1} -> ${pair._2}").mkString(", ")}")

    val selected = r.shuffle(byClass.flatMap{ case (lbl, accts) => r.shuffle(accts).take(desiredDims(lbl)) })

    selected.toSeq
  }

  def denoise(account: TwitterAccount): TwitterAccount = {
    val good = account.tweets.filterNot { tweet =>
      val txt = tweet.text.split(" +")
      val spammy = Seq("4sq", "instagr.am", "instagram.com", "fb.me", "#latergram", "#regram", "…")
      tweet.isRetweet || txt.exists(tok => spammy.exists(spamwd => tok.contains(spamwd)))
    }
    account.copy(tweets=good)
  }

  def isNoise(tweet: Tweet): Boolean = {
    val txt = tweet.text.split(" +")
    val spammy = Seq("4sq", "instagr.am", "instagram.com", "fb.me", "#latergram", "#regram", "…")
    tweet.isRetweet || txt.exists(tok => spammy.exists(spamwd => tok.contains(spamwd)))
  }

  def keepRows[L, F](dataset: Dataset[L, F], rowsToKeep: Array[Int]): RVFDataset[L, F] = {
    val ds = dataset.asInstanceOf[RVFDataset[L, F]]
    new RVFDataset(
      dataset.labelLexicon,
      dataset.featureLexicon,
      rowsToKeep.map(ds.labels.apply).to[mutable.ArrayBuffer],
      rowsToKeep.map(ds.features.apply).to[mutable.ArrayBuffer],
      rowsToKeep.map(ds.values.apply).to[mutable.ArrayBuffer]
    )
  }

  def dehashtag(wd: String): String = wd.replaceFirst("#", "")

  def sanitizeHandle(h: String) = h.replaceFirst("@", "")
}