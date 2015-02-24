package edu.arizona.sista.twitter4food

/**
 * Created by dfried on 1/14/14.
 */
object Ngrams {
  def ngrams[A](tokens: Seq[A], n: Int): Seq[List[A]] = {
    if (n == 1)
      tokens.map({a => List(a)})
    else if (n > 1)
      if (tokens.isEmpty) 
        Seq[List[A]]() 
      else
        Utils.collectionExtras(tokens).zipWith(ngrams(tokens.tail, n - 1))(_ :: _)
    else
      throw new IllegalArgumentException("in ngrams, n == " + n)
  }

  def ngramsStrings(tokens: Seq[String], n: Int): Seq[String] =
    ngrams(tokens, n).map(lst => lst.mkString("_"))
}
