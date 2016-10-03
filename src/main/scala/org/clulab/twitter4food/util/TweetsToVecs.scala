package org.clulab.twitter4food.util

import org.apache.commons.io.FilenameUtils
import com.typesafe.config.ConfigFactory
import org.clulab.twitter4food.struct.TwitterAccount
import org.clulab.twitter4food.util.FileUtils._

import scala.util.Random

import java.io._

object TweetsToVecs {

  /**
    * For each account, generate one row containing the indices for each token in each tweet. <br>
    * The indices are generated from a word2vec file that will be used by a neural net later.
    * @param accounts [[TwitterAccount]]s to encode
    * @param maxTokens Greatest number of tokens per tweet to encode (remainders padded with 0)
    * @param maxTweets Greatest number of tweets to encode (remainders padded with 0)
    * @return A tuple of the [[Map]] from index to word and the matrix with one account per row
    */
  def encode(accounts: Seq[TwitterAccount], maxTokens: Int = 100, maxTweets: Int = 200): (Map[Int, String], Seq[Seq[Int]]) = {

    val config = ConfigFactory.load
    val w2vLines = scala.io.Source.fromFile(config.getString("classifiers.features.foodVectors")).getLines()
    val (vocabSize, dims) = w2vLines.next

    // Iterate through w2v file to retrieve the words it contains. Ignore the actual values of each word
    var i = 2 // 0 and 1 are reserved; 0 = padding; 1 = out-of-vocabulary
    val w2v = (for {
        line <- w2vLines
      } yield {
        val ret = line.split(" ").head -> i
        i += 1
        ret
      }).toMap

    val matrix = for {
      account <- accounts
    } yield {
      // Be sure to pad each tweet's text with a beginning and ending marker
      // Note slices to limit the length of the Seq of Tweets and the tokens in each Tweet
      val allTokens = account
        .tweets
        .slice(0, maxTweets)
        .map(tweet =>
          "<s>" +:
            tweet.text.toLowerCase.split("\\s+").slice(0, maxTokens - 2) :+
            "</s>"
        )
      // Now look up each token in the w2v Map (1 means out-of-vocabulary)
      allTokens.flatMap { tweetTokens =>
        tweetTokens.map{ token =>
          w2v.getOrElse(token, 1)
        }.padTo(maxTokens, 0) // remove this padTo to have variable lengths of tweets
      }.padTo(maxTweets * maxTokens, 0)
    }

    (w2v.map(_.swap), matrix)
  }

  def main(args: Array[String]): Unit = {
    assert(args.length > 0, "Usage is TweetsToVecs <infile>")

    val infile = args(0)

    val f = new File(infile)
    val justFile = FilenameUtils.getBaseName(infile)
    val outdir = f.getParent // to get the parent dir

    val labeledAccts = load(infile).toSeq
    val r = new Random(11111117)
    val shuffledAccts = r.shuffle(labeledAccts)

    val (accounts, labels) = shuffledAccts.unzip
    val intLabels = labels map {
      case "Overweight" => 1
      case not => 0
    }

    val (vocab, x) = encode(accounts)

    var writer = new BufferedWriter(new FileWriter(outdir + justFile + ".vocab"))
    vocab.foreach{ case (i, s) => writer.write(s"$i\t$s\n") }
    writer.close()

    writer = new BufferedWriter(new FileWriter(outdir + justFile + ".X"))
    x.foreach{ row => writer.write(s"${row.mkString}\n") }
    writer.close()

    writer = new BufferedWriter(new FileWriter(outdir + justFile + ".y"))
    intLabels.foreach{ label => writer.write(s"$label\n") }
    writer.close()
  }
}