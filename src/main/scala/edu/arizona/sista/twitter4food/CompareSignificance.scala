package edu.arizona.sista.twitter4food

import com.github.tototoshi.csv.CSVReader
import edu.arizona.sista.utils.EvaluationStatistics

/**
 * Created by df345 on 09/07/15.
 */
object CompareSignificance {

  def parse(filename: String, header: Boolean = true): List[(String, String, String)] = {
    val reader = CSVReader.open(filename)
    val lines = reader.iterator.toList
    val result = (if (header) lines.drop(1) else lines).map(fields => (fields(0), fields(1), fields(2)))
    reader.close()
    result
  }

  def main(args: Array[String]): Unit = {

    val control = parse(args(0))
    val treatment = parse(args(1))

    require(control.map(_._1) == treatment.map(_._1), "labels must match")
    require(control.map(_._2) == treatment.map(_._2), "actual labels must match")

    val actual = control.map(_._2)

    val controlPredicted = control.map(_._3)
    val treatmentPredicted = control.map(_._3)

    val controlAccuracy = (actual zip controlPredicted).filter(p => p._1 == p._2).size.toDouble / controlPredicted.size
    val treatmentAccuracy = (actual zip treatmentPredicted).filter(p => p._1 == p._2).size.toDouble / treatmentPredicted.size

    val pvalue = EvaluationStatistics.classificationAccuracySignificance(treatmentPredicted, controlPredicted, actual)
    println(s"control accuracy: ${controlAccuracy}")
    println(s"treatment accuracy: ${treatmentAccuracy}")
    println(s"p = ${pvalue}")
  }

}
