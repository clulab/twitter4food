package org.clulab.twitter4food

import org.clulab.struct.Lexicon

object CreateLexicon {
  def main(args: Array[String]) = {
    if(args.length < 2) throw new RuntimeException("Must include i/o filenames")
    val lines = scala.io.Source.fromFile(args(0)).getLines
    val lex = new Lexicon[String]
    lines.foreach(l => lex.add(l.split("\\s+")(0).toLowerCase))
    lex.saveTo(args(1))

    val _lex = Lexicon.loadFrom[String](args(1))
    assert(lex == _lex)
  }
}