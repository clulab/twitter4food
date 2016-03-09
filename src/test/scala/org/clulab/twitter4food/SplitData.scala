package org.clulab.twitter4food

import scala.io.Source
import com.typesafe.config.ConfigFactory
import java.io._

object SplitData extends App {
  val config = ConfigFactory.load()
  val accounts = Source.fromFile(config.getString("classifiers.gender.trainingFile"))
    .getLines.toList
  val (males, females) = accounts.reverse.foldLeft((List[String](), List[String]()))(
    (l, a) => {
      if(a.split("\t")(2).equals("M")) (a::l._1, l._2) else (l._1, a::l._2)
      })
  println(males.size + " " + females.size)
  
  val trainingSet = males.slice(0, 270) ++ females.slice(0, 330)
  val devSet = males.slice(270, 360) ++ females.slice(330, 440)
  val testSet = males.slice(360, 450) ++ females.slice(440, 550)

  println(s"${trainingSet.size}, ${devSet.size}, ${testSet.size}")
  val path1 = s"${config.getString("resources")}/${config.getString("default_package")}/"
  val path = path1 + "featureclassifier/gender/"
  val trainWriter = new BufferedWriter(new FileWriter(new File(path+"trainingSet.txt")))
  val devWriter = new BufferedWriter(new FileWriter(new File(path+"devSet.txt")))
  val testWriter = new BufferedWriter(new FileWriter(new File(path+"testSet.txt")))

  try {
    trainingSet.foreach(t => trainWriter.write(t.split("\t")(0) + "\t" + t.split("\t")(2) + "\n"))
    devSet.foreach(t => devWriter.write(t.split("\t")(0) + "\t" + t.split("\t")(2) + "\n"))
    testSet.foreach(t => testWriter.write(t.split("\t")(0) + "\t" + t.split("\t")(2) + "\n"))

    trainWriter.close()
    devWriter.close()
    testWriter.close()
    } catch {
      case e: IOException => e.printStackTrace()
    }

}