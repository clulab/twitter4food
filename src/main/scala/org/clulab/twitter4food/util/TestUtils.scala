package org.clulab.twitter4food.util

import org.clulab.twitter4food.twitter4j._
import org.clulab.twitter4food.struct._
import java.io._
import com.typesafe.config.ConfigFactory
import scala.reflect.ClassTag

object TestUtils {
  def init(keyset: Int, isAppOnly: Boolean) = {
    (new TwitterAPI(keyset, isAppOnly), ConfigFactory.load())
  }

  def loadHandles(fileName: String) = {
    scala.io.Source.fromFile(fileName).getLines.toList
      .foldLeft(Map[String, String]())(
        (map, line) => map + (line.split("\t")(0) -> line.split("\t")(1)))
  }

  def splitHandles[T: ClassTag](keyset: Int, numWindows: Int, 
    collection: Map[T, String]): (Array[T], Array[String]) = {
    val window = collection.size/numWindows
    val subHandles = collection.keys.slice(keyset*window, (keyset+1)*window)
      .toArray
    val subLabels = subHandles.map(k => collection(k))

    (subHandles -> subLabels)
  }

  def fetchAccounts(api: TwitterAPI, handles: Seq[String], 
    fetchTweets: Boolean) = {
    handles.map(h => api.fetchAccount(h, fetchTweets))
  }
}