package org.clulab.twitter4food

import scala.io.Source
import com.typesafe.config.ConfigFactory
import java.io._
import org.clulab.twitter4food.util._
import org.clulab.twitter4food.struct._

object SplitData extends App {
  val (api, config) = TestUtils.init(0, true)
  /*val accounts = Source.fromFile(config.getString("classifiers.gender.trainingFile"))
    .getLines.toList
  val (males, females) = accounts.reverse.foldLeft((List[String](), List[String]()))(
    (l, a) => {
      if(a.split("\t")(2).equals("M")) (a::l._1, l._2) else (l._1, a::l._2)
      })*/

  val accounts = scala.collection.mutable.Map[TwitterAccount, String]()
  for(i <- 0 to 7)
    accounts ++= FileUtils.load(config.getString("classifiers.gender.opt")+i+".txt")

  val (males, females) = accounts.foldLeft((List[(TwitterAccount, String)](),
    List[(TwitterAccount, String)]()))(
    (list, account) => {
      if(account._2.equals("M")) ((account._1, account._2) :: list._1, list._2)
      else (list._1, (account._1, account._2) :: list._2)
      })
  println(males.size + " " + females.size)
  
  val trainingSplit = males.slice(0, 270) ++ females.slice(0, 330)
  val trainData = trainingSplit.map(t => t._1)
  val trainLabels = trainingSplit.map(t => t._2)

  val devSplit = males.slice(270, 360) ++ females.slice(330, 440)
  val devData = devSplit.map(t => t._1)
  val devLabels = devSplit.map(t => t._2)


  val testSplit = males.slice(360, 450) ++ females.slice(440, 550)
  val testData = testSplit.map(t => t._1)
  val testLabels = testSplit.map(t => t._2)


  println(s"${trainingSplit.size}, ${devSplit.size}, ${testSplit.size}")
  val path1 = s"${config.getString("resources")}/${config.getString("default_package")}/"
  val path = path1 + "featureclassifier/gender/"
  
  val trainDataPath = path + "trainingData.txt"
  val devDataPath = path + "devData.txt"
  val testDataPath = path + "testData.txt"

  FileUtils.saveToFile(trainData, trainLabels, trainDataPath)
  FileUtils.saveToFile(devData, devLabels, devDataPath)
  FileUtils.saveToFile(testData, testLabels, testDataPath)

}