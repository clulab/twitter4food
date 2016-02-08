package org.clulab.twitter4food

class LabelCount {
  var humans = 0
  var orgs = 0
  var unknowns = 0
  def +=(b: LabelCount): LabelCount = { 
      this.humans += b.humans
      this.orgs += b.orgs
      this.unknowns += b.unknowns
      this
  }

  def total = humans + orgs + unknowns
}

object TestAnalyzeResults {
  def main(args: Array[String]): Unit = {
    val OPT = "opt/usersMidrangePredictedLabels_"
    val counts: LabelCount = new LabelCount()
    for(i <- 0 to 15) {
      val fin = OPT + i + ".txt"
      val results = scala.io.Source.fromFile(fin).getLines
                                   .map(x => {
                                      val s = x.split("\t"); (s(0), s(1))
                                    })
                                   .toArray
      counts += results.foldLeft(new LabelCount()) {
                     (c, r) => r._2 match {
                                 case "human" => c.humans += 1; c
                                 case "org" => c.orgs += 1; c
                                 case "unknown" => c.unknowns += 1; c
                               }
                }
      
    }
    
    print(s"numHumans : ${counts.humans}\t")
    print(s"numOrgs : ${counts.orgs}\t")
    print(s"numUnknowns : ${counts.unknowns}\t")
    println(s"total : ${counts.total}")

    val out = new java.text.DecimalFormat("#.###")
    print(s"humans : ${out.format(counts.humans.toDouble/counts.total*100)}%\t")
    print(s"orgs : ${out.format(counts.orgs.toDouble/counts.total*100)}%\t")
    println(s"unknowns : ${out.format(counts.unknowns.toDouble/counts.total*100)}%")
  }
}