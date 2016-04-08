package org.clulab.twitter4food

import org.clulab.twitter4food.util._

object TestDownloadAccounts {
  def main(args: Array[String]): Unit = {
    val keyset = args(0).toInt
    val numWindows = args(1).toInt
    val (api, config) = TestUtils.init(keyset, true)
    val hlMap = TestUtils.loadHandles(config
      .getString("classifiers.human.annotatedUsersFile"))
    val (subH, subL) = TestUtils.splitHandles(keyset, numWindows, hlMap)
    val accounts = TestUtils.fetchAccounts(api, subH, true)

    FileUtils.saveToFile(accounts, subL, 
      config.getString("classifiers.human.opt")+keyset+".txt")
  }
}