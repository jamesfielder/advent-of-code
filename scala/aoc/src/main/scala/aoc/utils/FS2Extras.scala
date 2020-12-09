/*
 * Copyright (c) 2020 James Fielder.
 * All rights reserved.
 */

package aoc.utils

import fs2._
import monix.eval.Task

object FS2Extras {

  implicit class FS2Extras[+O](s: Stream[Task, O]) {
    def getLastValueAsTask: Task[O] =
      s.pull
        .last
        .flatMap(f => Pull.output1(f))
        .stream.compile.toList.map(_.head.get)
  }

}
