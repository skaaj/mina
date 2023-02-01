package net.skaaj.core

import scala.util.chaining.*

object Utils {
  def time[R](block: => R): (Long, R) = {
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    ((t1 - t0) / 1_000 , result)
  }

  def timeTap[R](f: ((Long, R)) => Any)(block: => R): R = {
    time(block).tap(f)(1)
  }
}