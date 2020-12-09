/*
 * Copyright (c) 2020 James Fielder.
 * All rights reserved.
 */

package aoc.utils

import cats.effect.{Blocker, ContextShift, Sync}
import fs2._
import monix.eval.Task

import java.nio.file.Paths

trait FileUtils {

  def readFilesToInt[F[_] : Sync](path: String)(implicit cs: ContextShift[F]): Stream[F, Int] =
    readFileStream(path).map(_.toInt)

  def readFilesToLong[F[_] : Sync](path: String)(implicit cs: ContextShift[F]): Stream[F, Long] =
    readFileStream(path).map(_.toLong)

  def readFileStream[F[_] : Sync](path: String)(implicit cs: ContextShift[F]): Stream[F, String] =
    Stream.resource(Blocker[F]).flatMap { blocker =>
      io.file.readAll[F](Paths.get(path), blocker, 4096)
        .through(text.utf8Decode)
        .through(text.lines)
    }

  def readFileFast(path: String): Task[Iterator[String]] = Task(scala.io.Source.fromFile(path).getLines())
}
