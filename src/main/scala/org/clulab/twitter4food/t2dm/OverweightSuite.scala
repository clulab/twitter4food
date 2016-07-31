package org.clulab.twitter4food.t2dm

import java.io.{BufferedWriter, FileWriter}
import java.nio.file.{Files, Paths}

import com.typesafe.config.ConfigFactory
import org.clulab.learning.L1LinearSVMClassifier
import org.clulab.twitter4food.struct.TwitterAccount
import org.clulab.twitter4food.util.{Eval, FileUtils}
import org.slf4j.LoggerFactory

object OverweightSuite {
  def main(args: Array[String]): Unit = {
    val logger = LoggerFactory.getLogger(this.getClass)
    val config = ConfigFactory.load

    logger.info("Loading training accounts...")
    val trainingData = FileUtils.load(config.getString("classifiers.overweight.trainingData"))

    logger.info("Loading dev accounts...")
    val testSet: Map[TwitterAccount, String] = if (args.contains("-t"))
      FileUtils.load(config.getString("classifiers.overweight.testData"))
    else
      FileUtils.load(config.getString("classifiers.overweight.devData"))

    for {
      // useBigrams <- Seq(false, true)
      useTopics <- Seq(false, true)
      useDictionaries <- Seq(false, true)
      useEmbeddings <- Seq(false, true)
      useCosineSim <- Seq(false, true)
      useFollowers <- Seq(false, true)
      useFollowees <- Seq(false, true)
      useGender <- Seq(false, true)
      // useRace <- Seq(false, true)
      datumScaling <- Seq(false, true)
    } {
      val opts = Seq(
        "u",
        // if (useBigrams) "b" else "",
        if (useTopics) "t" else "",
        if (useDictionaries) "d" else "",
        if (useEmbeddings) "e" else "",
        if (useCosineSim) "c" else "",
        if (useFollowers) "f" else "",
        if (useFollowees) "h" else "",
        if (useGender) "g" else "",
        // if (useRace) "r else "",
        if (datumScaling) "D" else ""
      )
      val fileExt = opts.sorted.mkString("")
      val outputDir = config.getString("classifier") + "/overweight/results/" + fileExt
      if (!Files.exists(Paths.get(outputDir))) Files.createDirectories(Paths.get(outputDir))
      val modelFile = s"${config.getString("overweight")}/model/${fileExt}.dat"

      val oc = new OverweightClassifier(
        useUnigrams = true,
        useBigrams = useBigrams,
        useTopics = useTopics,
        useDictionaries = useDictionaries,
        useCosineSim = useCosineSim,
        useFollowers = useFollowers,
        useFollowees = useFollowees,
        useGender = useGender,
        datumScaling = datumScaling
      )

      // Train classifier and save model to file
      logger.info("Training classifier...")
      oc.setClassifier(new L1LinearSVMClassifier[String, String]())
      oc.train(trainingData.keys.toSeq, trainingData.values.toSeq)
      oc.subClassifier.get.saveTo(modelFile)

      // Set progress bar
      val pb = new me.tongfei.progressbar.ProgressBar("main()", 100)
      pb.start()
      pb.maxHint(testSet.size)
      pb.setExtraMessage("Testing on dev accounts...")

      // Classify accounts
      val testSetLabels = testSet.values.toSeq
      val predictedLabels = testSet.keys.toSeq.map(u => { pb.step(); oc.classify(u); })

      pb.stop()

      // Print results
      val (evalMeasures, microAvg, macroAvg) = Eval.evaluate(testSetLabels, predictedLabels,
        testSet.keys.toSeq)
      val evalMetric = evalMeasures("Overweight")
      val precision = evalMetric.P
      val recall = evalMetric.R

      println(fileExt)
      println(s"Precision: $precision")
      println(s"Recall: $recall")
      println(s"F-measure (harmonic mean): ${OverweightClassifier.fMeasure(precision, recall, 1)}")
      println(s"F-measure (recall 5x): ${OverweightClassifier.fMeasure(precision, recall, .2)}")
      println(s"Macro average: $macroAvg")
      println(s"Micro average: $microAvg\n")

      // Save results
      val writer = new BufferedWriter(new FileWriter(outputDir + "/analysisMetrics.txt", false))
      writer.write(s"Precision: $precision\n")
      writer.write(s"Recall: $recall\n")
      writer.write(s"F-measure (harmonic mean): ${OverweightClassifier.fMeasure(precision, recall, 1)}\n")
      writer.write(s"F-measure (recall 5x): ${OverweightClassifier.fMeasure(precision, recall, .2)}\n")
      writer.write(s"Macro average: $macroAvg\n")
      writer.write(s"Micro average: $microAvg\n")
      writer.close()

      // Save individual predictions for bootstrap significance
      val predicted = new BufferedWriter(new FileWriter(outputDir + "/predicted.txt", false))
      predicted.write(s"gold\tpred\n")
      testSetLabels.zip(predictedLabels).foreach(acct => predicted.write(s"${acct._1}\t${acct._2}\n"))
      predicted.close()
    }
  }
}