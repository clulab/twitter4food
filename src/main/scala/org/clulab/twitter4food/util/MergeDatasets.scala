package org.clulab.twitter4food.util

import org.clulab.twitter4food.util.FileUtils._

/**
  * Reads two or more TwitterAccount files (two lines per tweet) and merges them. <br>
  * Usage is MergeDatasets file1.txt file2.txt ... outfile.txt
  *
  * @author Dane Bell
  */
object MergeDatasets {
  def main(args: Array[String]): Unit = {
    assert(args.length > 2, "Usage is 'MergeDatasets file1.txt file2.txt ... outfile.txt'")
    assert(args.length == args.distinct.length, "Attempting to merge identical files or overwrite existing file!")

    val datasets = (for (f <- args.dropRight(1)) yield load(f)).flatten

    // Hopefully, no accounts will overlap between the datasets, but if they do,
    // we want all the tweets we can get from them.
    val ids = datasets.unzip._1.map(_.id)
    val (overlapping, nonOverlapping) = ids
      .groupBy(identity)
      .mapValues(_.length)
      .partition{ case (handle, number) => number > 1}

    // Merge accounts that have the same handles
    val mergedAccounts = for (ol <- overlapping.keys) yield {
      val unmerged = datasets.filter{ case (acct, lbl) => acct.id == ol}
      val labels = unmerged.map(_._2).distinct
      assert(labels.length == 1, s"A single account ${unmerged.head._1.handle} has multiple labels!")
      (unmerged.map(_._1).reduce( (a, b) => a.merge(b)), labels.head)
    }

    val singletonAccounts = datasets.filter{ case (acct, lbl) => nonOverlapping.keys.toSeq contains acct.id }

    val (newAccounts, newLabels) = (mergedAccounts ++ singletonAccounts).filter(_._2 != "Can't tell").unzip

    saveToFile(newAccounts.toSeq, newLabels.toSeq, args.last, append = false)
  }
}