package org.clulab.twitter4food

import org.clulab.twitter4food.featureclassifier.HumanClassifier
import org.clulab.twitter4food.twitter4j.TwitterAPI

import java.io.{BufferedWriter, File, FileWriter, IOException}


// TODO refactor to use new read-only TwitterAcount
/**
  * Created by adikou on 1/21/16.
  */
object TestTwitterAPI {
  def main (args: Array[String]) {

//    val USERS = "src/main/resources/org/clulab/twitter4food/featureclassifier/usersMidrange.txt"
//    val classifier = new HumanClassifier()
//    classifier.init()
//    classifier.train()
//
//    val keyset = args(0).toInt
//    val numWindows = args(1).toInt
//
//    val allUsers = scala.io.Source.fromFile(USERS)
//                                  .getLines.map(x => x.split("\t")(0))
//                                  .toArray
//    val N = allUsers.size
//    val window = N/numWindows
//
//    val accounts = allUsers.slice(keyset * window, (keyset + 1) * window).toArray
//
//    //println(window + " " + N)
//    //println(keyset * window, (keyset + 1) * window)
//
//    val APIInstance = new TwitterAPI(keyset)
//
//    val writer = new BufferedWriter(new FileWriter(
//                                    new File(s"opt/usersMidrangePredictedLabels_${keyset}.txt")))
//
//    val cache = new BufferedWriter(new FileWriter(
//                                   new File(s"opt/accountCache_${keyset}.txt")))
//
//    val labels = accounts.foldLeft(List[String]()) {
//                   (labelSubList, account) =>  {
//		     val user = APIInstance.fetchAccount(account)
//         if(user != null) {
//           println(user.getName)
//           try {
//
//             cache.write(user.getHandle + "\t" + user.getName + "\t")
//             cache.write(user.getId + "\n")
//             cache.write(user.getDescription + "\n")
//             cache.write(user.getLang + "\t" + user.getUrl + "\t")
//             cache.write(user.getLocation + "\n")
//           } catch {
//             case io: IOException => io.printStackTrace
//           }
//         }
//
//         if(user != null)
//           classifier.classify(user)::labelSubList
//         else classifier.labels(classifier.UNKNOWN)::labelSubList
//      }
//    }
//
//    cache.close()
//
//    try {
//      for(i <- 0 until accounts.length)
//        writer.write(accounts(i) + "\t" + labels(i) + "\n")
//
//      writer.flush()
//      writer.close()
//    } catch {
//      case io: IOException => io.printStackTrace()
//    }
  }
}
