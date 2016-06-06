package org.clulab.twitter4food

import java.io.File
import org.clulab.twitter4food.struct._
import org.clulab.twitter4food.twitter4j._
import org.clulab.twitter4food.util._
import com.typesafe.config.ConfigFactory

object TestFileUtils {
  def main(args: Array[String]): Unit = {
    val keyset = args(0).toInt
    val numWindows = args(1).toInt
    val api = new TwitterAPI(keyset)
    val config = ConfigFactory.load()
    val allAccounts = scala.io.Source.fromFile(
      config.getString("classifiers.gender.annotatedUsersFile")).getLines
      .toList.foldLeft(Map[String, String]())(
        (map, line) => map + (line.split("\t")(0) -> line.split("\t")(2)))
    val N = allAccounts.size
    val window = N/numWindows
    val handles = allAccounts.keys.slice(keyset * window,(keyset + 1)*window).toArray
    val labels = handles.map(k => allAccounts(k))

    val accounts = handles.map(h => api.fetchAccount(h, fetchTweets=true))
    val activeAccounts = accounts.foldLeft(Map[TwitterAccount, String]())(
      (map, account) => {
        if(account != null)
          map + (account -> labels(handles.indexOf(account.handle)))
        else map
        })
    val activeHandles = activeAccounts.keys.toArray
    val activeLabels = activeHandles.map(k => activeAccounts(k))


    FileUtils.saveToFile(activeHandles, activeLabels, 
      config.getString("classifiers.gender.opt") + keyset + ".txt")
  }
}