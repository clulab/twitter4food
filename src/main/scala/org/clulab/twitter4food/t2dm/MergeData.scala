package org.clulab.twitter4food.t2dm

import com.typesafe.config.ConfigFactory
import org.clulab.twitter4food.util.FileUtils

/**
  * Created by Terron on 6/29/16.
  */
object MergeData {
    def main(args: Array[String]) {
        val config = ConfigFactory.load()
        val myAccounts = FileUtils.load(config.getString("classifiers.overweight.data"))
        val adityaAccounts = FileUtils.load(config.getString("aditya"))
        val adityaHandles = adityaAccounts.keys.map(a => a.handle).toSet
        val myFilteredAccounts = myAccounts.filter({
            case (k, v) => !adityaHandles.contains(k.handle)
        })
        val toWrite = adityaAccounts ++ myFilteredAccounts
        FileUtils.saveToFile(toWrite.keys.toList, toWrite.values.toList, "tmp.txt")
    }
}
