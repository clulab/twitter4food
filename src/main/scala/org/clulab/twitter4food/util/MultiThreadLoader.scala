package org.clulab.twitter4food.util

import org.clulab.twitter4food.struct._
import scala.collection.mutable.ArrayBuffer
import java.util.concurrent.{Executors, ExecutorService}
import java.util.concurrent.{Callable, CyclicBarrier, Future}

/*
 * Call as val accounts = (new MultiThreadLoader(_, _, _, numThreads)).call
 */

class MultiThreadLoader(handles: Seq[String], isAppOnly: Boolean,
  fetchTweets: Boolean, poolSize: Int) extends Callable[Seq[TwitterAccount]] {
  val pool = Executors.newFixedThreadPool(poolSize)
  def call(): Seq[TwitterAccount] = {
    val futureSet = ArrayBuffer[Future[Seq[TwitterAccount]]]()
    try { 
      for(i <- 0 until poolSize) {
        futureSet += pool.submit(new ThreadRunner(handles, isAppOnly, 
          fetchTweets, poolSize))
      }
    } catch {
      case ie: InterruptedException => println("Something got interrupted")
    } finally {
      pool.shutdown()
    }

    futureSet.foldLeft(Array[TwitterAccount]())(
      (accounts, future) => accounts ++ future.get)
  }
}

class ThreadRunner(handles: Seq[String], isAppOnly: Boolean,
  fetchTweets: Boolean, N: Int) extends Callable[Seq[TwitterAccount]] {
  def call(): Seq[TwitterAccount] = {
    val id = Thread.currentThread.getId.toInt % N
    val (api, config) = TestUtils.init(id, isAppOnly)
    val hlMap = handles.foldLeft(Map[String, String]())(
      (map, h) => map + (h -> ""))
    val (subH, subL) = TestUtils.splitHandles(id, N, hlMap)
    val accounts = TestUtils.fetchAccounts(api, subH, fetchTweets)

    accounts
  }
}
