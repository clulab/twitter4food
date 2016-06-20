package edu.arizona.sista.twitter4food

import org.clulab.struct.Counter
import java.util.regex.Pattern
import scala.collection.mutable.ListBuffer
import java.io.{FileWriter, PrintWriter}

/**
 *
 * User: mihais
 * Date: 10/25/13
 */
class GeoTagger(val statePatterns: Map[String, List[Pattern]]) {

  def this(stateFilename: String) =
    this(GeoTag.loadStatePatterns(stateFilename))

  def this(source: io.Source = io.Source.fromURL(GeoTagger.stateResource)) =
    this(GeoTag.loadStatePatterns(source))

  def normalizeLocation(loc:String, timeZone:String):Option[String] = GeoTag.normalizeLocation(loc, timeZone,
    statePatterns)
}

object GeoTagger {
  val stateResource = getClass.getResource("states.txt")
}

object GeoTag {
  def main(args:Array[String]) {
    val stateFile = args(0)
    val in_file = args(1)
    val out_file = args(2)

    val states = loadStatePatterns(stateFile)
    //println(normalizeLocation("los angeles, ca", states))
    //println(normalizeLocation("new york, ny", states))

    val os = new PrintWriter(new FileWriter(out_file))
    geoTagTweets(states, in_file, os)
    os.close()
  }

  def geoTagTweets(states:Map[String, List[Pattern]], file:String, os:PrintWriter) {
    var lineCount = 0
    var tweetsWithLoc = 0
    var totalTweets = 0
    var tweetsNormed = 0
    val locStats = new Counter[String]()
    val stateStats = new Counter[String]()
    val unnormLocs = new Counter[String]()
    var metaLine:String = null
    for (line <- Utils.loadFile(file).getLines) {
      lineCount += 1
      if(lineCount % 3 == 1) {
        metaLine = line
      } else if(lineCount % 3 == 0) {
        totalTweets += 1
        val bits = metaLine.split('\t')
        val loc = bits(3).toLowerCase()
        val timeZone = bits(6).toLowerCase()
        locStats.incrementCount(loc)
        if(loc != "nil") {
          tweetsWithLoc += 1
          normalizeLocation(loc, timeZone, states) match {
            case Some(state) => {
              tweetsNormed += 1
              stateStats.incrementCount(state)
              // os.println(state + "\t" + line.replaceAll("\\s+", " "))
            }
            case None => unnormLocs.incrementCount(loc)
          }
        }
      }
    }

    val showLocs = false
    if(showLocs) {
      for(t <- locStats.sorted) {
        if(t._2 > 1) {
          println(t._1 + "\t" + t._2)
        }
      }
    }

    val showStates = false
    if(showStates) {
      for(t <- stateStats.sorted) {
        println(t._1 + "\t" + t._2)
      }
    }

    val showUnnorm = false
    if(showUnnorm) {
      for(t <- unnormLocs.sorted) {
        if(t._2 > 10) {
          println(t._1 + "\t" + t._2)
        }
      }
    }

    /*
    println("Total tweets: " + totalTweets)
    println("Tweets with location info: " + tweetsWithLoc)
    println("Tweets that could be normalized: " + tweetsNormed)
    */
  }

  val USA_SUFFIX = Pattern.compile("([\\s*,]?\\s*USA?\\s*$)|([\\s*,]?\\s*united\\s*states\\s*(of\\s*america)?\\s*$)", Pattern.CASE_INSENSITIVE)

  def normalizeLocation(loc:String, timeZone:String, states:Map[String, List[Pattern]]):Option[String] = {
    // remove any potential USA suffixes
    val m = USA_SUFFIX.matcher(loc)
    var l = loc
    if(m.matches()) {
      l = loc.substring(0, m.start())
      //println(s"STRIPPED USA: [$loc] to [$l]")
    }

    if(l == "") return None

    // special case: la
    if(loc == "la") {
      if(timeZone == null) {
        // can't normalize this one
        return None
      } else if(timeZone.contains("pacific")) {
        //println("LA NORMALIZED TO CA")
        return Some("CA")
      } else if(timeZone.contains("central")) {
        //println("LA NORMALIZED TO LA")
        return Some("LA")
      } else {
        return None
      }
    }

    // return the first state that matches the location
    for(st <- states.keys) {
      for(p <- states(st)) {
        if(p.matcher(l).find()) {
          //println(s"[$l] normalized to [$st]")
          return Some(st)
        }
      }
    }
    None
  }

  def loadStatePatterns(source: io.Source) = {
    val states = new collection.mutable.HashMap[String, List[Pattern]]
    var count = 0
    for (line <- source.getLines) {
      count += 1
      val bits = line.split('\t')
      val st = bits(0).trim
      val pb = new ListBuffer[Pattern]
      for(i <- 1 until bits.length) {
        val core = bits(i).trim
        pb += Pattern.compile("\\s+" + core + "\\s*$", Pattern. CASE_INSENSITIVE)
        pb += Pattern.compile("^" + core + "\\s*" + "((,\\s*)?" + st + ")?" + "$", Pattern.CASE_INSENSITIVE)
      }
      states += st -> pb.toList
      // println("PATTERNS FOR " + st + ": " + pb.toList)
    }
    // println(s"Loaded $count states.")
    states.toMap
  }

  def loadStatePatterns(file:String): Map[String, List[Pattern]] = {
    loadStatePatterns(io.Source.fromFile(file))
  }
}
