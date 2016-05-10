package org.clulab.twitter4food.t2dm

import java.io.{BufferedReader, FileReader, PrintWriter}

import cmu.arktweetnlp.Tagger.TaggedToken
import org.clulab.twitter4food.util.{TestUtils, Tokenizer}

/**
  * Created by Terron on 4/28/16.
  *
  * A simple script for parsing a file of tweets and writing to a file of aggregate text
  * for the w2v C script. Assumes every third line is a tweet, as is standard for this project.
  */
object Word2VecFormatter {

    def main(args: Array[String]) {
        val (_, config) = TestUtils.init(0)
        val overweightFile = scala.io.Source.fromFile(config.getString("classifiers.features.overweight_corpus"))

        val outputFile = config.getString("classifiers.features.overweight_corpus_tokenized")
        val writer = new PrintWriter(outputFile)

        var i = 0
        for (line <- overweightFile.getLines) {
            // Line is a tweet
            if (i % 3 == 2) {
                val tokens = Tokenizer.annotate(line.toLowerCase).filter(tt => !("@UGD,~$".contains(tt.tag))
                    && "#NVAT".contains(tt.tag)).map(tt => tt.token)
                writer.write(tokens.mkString(" ") + " ")
            }

            i += 1
            i %= 3
        }
        writer.close()
        overweightFile.close()
    }
}
