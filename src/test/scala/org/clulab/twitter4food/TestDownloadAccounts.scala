package org.clulab.twitter4food

import org.clulab.twitter4food.util._

object TestDownloadAccounts {
  def main(args: Array[String]): Unit = {
    val keyset = args(0).toInt
    val numWindows = args(1).toInt
    val (api, config) = Utils.init(keyset)
    val hlMap = Utils.loadHandles(s"${config.getString("classifier")}/overweight/ow_accounts.txt")
    val (subH, subL) = Utils.splitHandles(keyset, numWindows, hlMap)
    val accounts = Utils.fetchAccounts(api, subH, true, true, true)

    FileUtils.saveToFile(accounts, subL, 
      config.getString("classifiers.overweight.opt")+keyset+".txt")
  }
}