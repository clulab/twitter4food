package edu.arizona.sista.twitter4food

import scala.io.BufferedSource
import java.util.zip.GZIPInputStream
import java.io._
import scala.collection.IterableLike
import scala.collection.generic.CanBuildFrom
import scala.util.matching.Regex
import concurrent.Future
import concurrent.ExecutionContext.Implicits.global
import edu.arizona.sista.struct.Counter
import Mixins._

/**
 * Created by dfried on 1/12/14.
 */
object Utils {
  /**
   * Load a possibly zipped file
   * @param filename
   * @return the open file as a BufferedSource
   */
  def loadFile(filename: String): BufferedSource = {
    if(filename.endsWith(".gz"))
      io.Source.fromInputStream(new GZIPInputStream(new BufferedInputStream(new FileInputStream(filename))))
    else
      io.Source.fromFile(filename)
  }

  def time[R](code: => R): R = {
    val startTime = System.currentTimeMillis()
    val result: R = code
    val endTime = System.currentTimeMillis()
    println("elapsed time " + (endTime - startTime))
    result
  }

  def isURLToken(token: String): Boolean = token.startsWith("http")

  // http://stackoverflow.com/questions/3895813/how-to-write-a-zipwith-method-that-returns-the-same-type-of-collection-as-those
  implicit def collectionExtras[A, CC[A] <: IterableLike[A, CC[A]]](xs: CC[A]) = new {
    def zipWith[B, C, That](ys: Iterable[B])(f: (A, B) => C)(implicit cbf: CanBuildFrom[CC[A], C, That]) = {
      val builder = cbf(xs.repr)
      val (i, j) = (xs.iterator, ys.iterator)
      while(i.hasNext && j.hasNext) {
        builder += f(i.next, j.next)
      }
      builder.result
    }
  }

  // http://stackoverflow.com/questions/4604237/how-to-write-to-a-file-in-scala
  def printWithWriter(p: java.io.PrintWriter)(op: java.io.PrintWriter => Unit) {
    try { op(p) } finally { p.close() }
  }

  def printToFile(f: java.io.File) = printWithWriter(new java.io.PrintWriter(f)) _

  // allow chaining of conditionals
  // http://stackoverflow.com/questions/7313604/conditional-invocation-of-a-method-in-scala
  // usage: "fish".when(true)(_.upperCase) => "FISH"
  // usage: "fish".when(_.length<5)(_.upperCase) => "FISH"
  class When[A](a: A) {
    def when(p: => Boolean)(g: A => A) : A= if (p) g(a) else a

    def when(f: A => Boolean): (A => A) => A = when(f(a)) _
  }
  implicit def whenever[A](a: A) = new When(a)

  def filterByRegex(p: Regex)(strings: Seq[String]) = {
    strings filter { case p() => true; case _ => false}
  }

  // list of futures to future of lists,
  // http://stackoverflow.com/questions/20307175/why-does-this-list-of-futures-to-future-of-list-transformation-compile-and-work
  def all[T](fs: List[Future[T]]): Future[List[T]] =
    fs.foldRight(Future(Nil:List[T]))((f, fs2) =>
      for {
        x <- f
        xs <- fs2
      } yield (x::xs))

  def indicator(predicate: => Boolean) = if (predicate) 1 else 0

  def serialize[A](obj: A, filename: String): Unit = {
    val fout = new FileOutputStream(filename)
    val oout = new ObjectOutputStream(fout)
    oout.writeObject(obj)
    oout.close
    fout.close
  }
  
  def deserialize[A](filename: String): A = {
    val fin = new FileInputStream(filename)
    val oin = new ObjectInputStream(fin)
    val obj = oin.readObject.asInstanceOf[A]
    oin.close
    fin.close
    obj
  }

}
