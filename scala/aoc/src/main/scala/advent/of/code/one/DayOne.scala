/*
 * Copyright (c) 2020 James Fielder.
 * All rights reserved.
 */

package advent.of.code.one

import advent.of.code.utils.Utils
import cats.effect.ExitCode
import cats.implicits._
import monix.eval.{Task, TaskApp}

object DayOne extends TaskApp with Utils {
  override def run(args: List[String]): Task[ExitCode] = {
    readFile()
      .map(l => {
        (l.combinations(2) ++ l.combinations(3)).filter(_.sum == 2020).map(_.product).toList
      })
      .flatMap(_.traverse(o => Task { println(o) }))
      .as(ExitCode.Success)
  }

  private def readFile() =
    readFilesToInt("files/1.txt").compile.toList
}
