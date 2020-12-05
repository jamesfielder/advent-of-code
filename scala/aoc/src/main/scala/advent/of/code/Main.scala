/*
 * Copyright (c) 2020 James Fielder.
 * All rights reserved.
 */

package advent.of.code

import cats.effect.ExitCode
import monix.eval.{Task, TaskApp}
import monix.reactive.Observable

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.io.StdIn

object Main extends TaskApp {

  def stream(period: FiniteDuration): Observable[LocalDateTime] =
    Observable
      .intervalAtFixedRate(period)
      .mapEval(_ => Task.delay(LocalDateTime.now()))

  override def run(args: List[String]): Task[ExitCode] =
    Task.suspend {
      println("\nClock:\n")

      stream(500.millis)
        .map(_.format(DateTimeFormatter.ofPattern("HH:mm:ss")))
        .mapEval(dt => Task(print(s"\r$dt")))
        .completedL
        .start
        .bracket(_ => Task(StdIn.readLine()))(_.cancel)
        .flatMap(_ => Task(println("\nBye!\n")))
        .as(ExitCode.Success)
    }
}
