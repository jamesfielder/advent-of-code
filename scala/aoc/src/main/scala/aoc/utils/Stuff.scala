/*
 * Copyright (c) 2020 James Fielder.
 * All rights reserved.
 */

package aoc.utils

object Stuff {
  def time[R](block: => R)(msg: String = ""): R = {
    val t0 = System.currentTimeMillis()
    val result = block
    val t1 = System.currentTimeMillis()
    println("Elapsed time: " + (t1 - t0) + "ms" + msg)
    result
  }
}
