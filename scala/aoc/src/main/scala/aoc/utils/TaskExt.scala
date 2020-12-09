/*
 * Copyright (c) 2020 James Fielder.
 * All rights reserved.
 */

package aoc.utils

import monix.eval.Task

/** Extension methods for monix task */
object TaskExt {

  def iterate[A](start: A)(condition: A => Boolean)(f: A => Task[A]): Task[A] = {
    if (condition(start)) f(start).flatMap(iterate(_)(condition)(f)) else Task.now(start)
  }
}
