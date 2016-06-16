package org.clulab.twitter4food

import scala.collection.mutable.ArrayBuffer
import java.io._

object SplitNames {
  def main(args: Array[String]) = {
    if(args.length < 1) throw new RuntimeException("Specify input file")
    val lines = scala.io.Source.fromFile(args(0)).getLines
    
    val mFirstOnly = ArrayBuffer[String]()
    val fFirstOnly = ArrayBuffer[String]()
    val mfFirstOnly = ArrayBuffer[String]()

    val mBoth = ArrayBuffer[String]()
    val fBoth = ArrayBuffer[String]()
    val mfBoth = ArrayBuffer[String]()

    lines.foreach(l => {
      val splits = l.split("\\s+")
      val (g, isLastOnly, name) = (splits(0), splits(1), splits(2))
      val flag = (isLastOnly == "LY")
      
      g match {
        case "MF" => if(flag) mfBoth += name else mfFirstOnly += name
        case "MO" => if(flag) mBoth += name else mFirstOnly += name
        case "FO" => if(flag) fBoth += name else fFirstOnly += name
        }
      })

    val mfoWriter = new BufferedWriter(new FileWriter("/work/adityak/male.first"))
    val ffoWriter = new BufferedWriter(new FileWriter("/work/adityak/female.first"))
    val mffoWriter = new BufferedWriter(new FileWriter("/work/adityak/both.first"))
    val mbWriter = new BufferedWriter(new FileWriter("/work/adityak/male.firstlast"))
    val fbWriter = new BufferedWriter(new FileWriter("/work/adityak/female.firstlast"))
    val mfbWriter = new BufferedWriter(new FileWriter("/work/adityak/both.firstlast"))

    mfoWriter.write(mFirstOnly.mkString("\n"))
    ffoWriter.write(mFirstOnly.mkString("\n"))
    mffoWriter.write(mFirstOnly.mkString("\n"))
    mbWriter.write(mFirstOnly.mkString("\n"))
    fbWriter.write(mFirstOnly.mkString("\n"))
    mfbWriter.write(mFirstOnly.mkString("\n"))

    mfoWriter.close()
    ffoWriter.close()
    mffoWriter.close()
    mbWriter.close()
    fbWriter.close()
    mfbWriter.close()
  }
}