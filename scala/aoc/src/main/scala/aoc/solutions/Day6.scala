/*
 * Copyright (c) 2020 James Fielder.
 * All rights reserved.
 */

package aoc.solutions

import aoc.utils.FileUtils
import cats.effect.ExitCode
import cats.kernel.Monoid
import monix.eval.{Task, TaskApp}

object Day6 extends TaskApp with FileUtils {
  override def run(args: List[String]): Task[ExitCode] =
    (part1 *> part2).as(ExitCode.Success)

  def part1 =
    readFileChunks
      .map(_.foldLeft("")((a, b) => a + b).toSet)
      .map(_.size)
      .compile.toVector.map(_.sum).map(println(_))

  def part2 =
    readFileChunks
      .map(_.map(p => Answers(p.toCharArray.toSet, 1)))
      .map(c => Monoid.combineAll(c.toList))
      .map(a => a.letters.size)
      .compile.toVector.map(_.sum).map(println(_))

  def readFileChunks =
    readFileStream("files/6.txt")
      .split(_.isBlank)
}

case class Answers(letters: Set[Char], size: Int)
object Answers {
  implicit def answersMonoid: Monoid[Answers] = new Monoid[Answers] {
    override def empty: Answers = Answers(('a' to 'z').toSet, 0)

    override def combine(x: Answers, y: Answers): Answers =
      Answers(letters = x.letters.intersect(y.letters), size = x.size + y.size)
  }
}
