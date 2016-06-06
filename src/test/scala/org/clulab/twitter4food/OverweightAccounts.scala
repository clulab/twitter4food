package org.clulab.twitter4food

import org.clulab.twitter4food.struct.Tweet
import org.clulab.twitter4food.util._
import scala.collection.mutable.ArrayBuffer
import java.io.{FileWriter, BufferedWriter, IOException}

object OverweightAccounts {

  def filter(pos_kw: Array[String], neg_kw: Array[String], 
    tweets: Array[Tweet]) = {
    
    tweets.foldLeft(List[Tweet]())(
      (t, l) => if(pos_kw.exists(l.text.toLowerCase.contains(_)) 
        && !neg_kw.exists(l.text.toLowerCase.contains(_))) l::t else t)
  }

  def main(args: Array[String]): Unit = {
    val config = TestUtils.init(0)._2
    val lines = scala.io.Source.fromFile(
      config.getString("classifiers.overweight.stream")).getLines.toList
    var count = 0
    var handle = ""
    var text = ""
    val buf = new ArrayBuffer[Tweet]()

    lines.foreach(line =>{
      count match {
        case 0 => handle = line.split("\t")(0)
        case 2 => buf += new Tweet(line, -1, "", new java.util.Date(), handle)
        case _ =>
        }
      count += 1; count %= 3;
      })

    val tweets = buf.toArray
    val pos_kw = Array("#fatguyproblems", "#fatgirlproblems", "#fatguytweets",
      "#effyourbodystandards", "#fatgirltweets", "#plussize").map(_.toLowerCase)
    val neg_kw = Array("#nsfw", "#milf", "#bbw", "#mature", "#NSFW", "#model", 
      "#fashion", "#porn", "#clothes", "#sex", "#sexy", "#shopping",
      "ass").map(_.toLowerCase)
    val subTweets = filter(pos_kw, neg_kw, tweets)
    val htagHandles =  pos_kw.map(k => {
      println(s"$k: ${subTweets.foldLeft(0)((s,t) => if(t.text.contains(k)) s+1 else s)}")
      (k, subTweets.foldLeft(Set[String]())((s,t) => if(t.text.contains(k)) s + t.handle else s))
      })

    val handles = subTweets.foldLeft(Set[String]())((h, l) => h + l.handle)
    val bw = new BufferedWriter(new FileWriter(
      s"${config.getString("classifier")}/overweight/ow_accounts_" +
      s"${(new java.util.Date()).toString.replace(" ", "_")}.txt"))
    val bw2 = new BufferedWriter(new FileWriter(
      s"${config.getString("classifier")}/overweight/ow_accounts_"+
      s"${(new java.util.Date()).toString.replace(" ", "_")}_2.txt"))
    try {
      handles.foreach(h => bw.write(s"$h\tUNK\n"))
      htagHandles.foreach{case (t,h) => bw2.write(s"$t\n${h.map(_+"\tUNK").mkString("\n")}\n")}
      bw.flush()
      bw2.flush()
      } catch {
        case e: IOException =>
      }

    bw.close()
  }
  
}