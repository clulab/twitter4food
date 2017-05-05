package org.clulab.twitter4food

import scala.io.Source
import com.typesafe.config.ConfigFactory
import java.io._
import org.clulab.twitter4food.util._
import org.clulab.twitter4food.struct._

object SplitData extends App {
  val (api, config) = Utils.init(0)
  /*val accounts = Source.fromFile(config.getString("classifiers.gender.trainingFile"))
    .getLines.toList
  val (males, females) = accounts.reverse.foldLeft((List[String](), List[String]()))(
    (l, a) => {
      if(a.split("\t")(2).equals("M")) (a::l._1, l._2) else (l._1, a::l._2)
      })*/

  val hlMap = Utils.loadHandles(config
      .getString("classifiers.human.annotatedUsersFile")).keys.toSet

  val accounts = FileUtils.loadTwitterAccounts(config.getString("classifiers.human.allTrainData"))
  val (human, org) = accounts.foldLeft((List[(TwitterAccount, String)](),
    List[(TwitterAccount, String)]()))(
    (list, account) => {
      if(account._2.equals("human")) ((account._1, account._2) :: list._1, list._2)
      else (list._1, (account._1, account._2) :: list._2)
      })
  println(human.size + " " + org.size)
  println(hlMap -- accounts.keys.map(_.handle).toSet)
  
  val trainingSplit = human.slice(0, 256) ++ org.slice(0, 256)
  val trainData = trainingSplit.map(t => t._1)
  val trainLabels = trainingSplit.map(t => t._2)

  val devSplit = human.slice(256, 288) ++ org.slice(256, 288)
  val devData = devSplit.map(t => t._1)
  val devLabels = devSplit.map(t => t._2)


  val testSplit = human.slice(288, 320) ++ org.slice(288, 320)
  val testData = testSplit.map(t => t._1)
  val testLabels = testSplit.map(t => t._2)


  println(s"${trainingSplit.size}, ${devSplit.size}, ${testSplit.size}")
  /*val path1 = s"${config.getString("resources")}/${config.getString("default_package")}/"
  val path = path1 + "featureclassifier/human/"*/
  val path = config.getString("server_path") + "/human/"
  
  val trainDataPath = path + "trainingData.txt"
  val devDataPath = path + "devData.txt"
  val testDataPath = path + "testData.txt"

  FileUtils.saveToFile(trainData, trainLabels, trainDataPath)
  FileUtils.saveToFile(devData, devLabels, devDataPath)
  FileUtils.saveToFile(testData, testLabels, testDataPath)

}