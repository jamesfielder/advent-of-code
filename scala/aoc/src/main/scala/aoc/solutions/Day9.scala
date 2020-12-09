/*
 * Copyright (c) 2020 James Fielder.
 * All rights reserved.
 */

package aoc.solutions

import aoc.utils.FS2Extras.FS2Extras
import aoc.utils.{FileUtils, TaskExt}
import cats.effect.ExitCode
import fs2._
import monix.eval.{Task, TaskApp}
import monix.execution.Scheduler

object Day9 extends TaskApp {
  implicit val s: Scheduler = super.scheduler // Annoying but it doesn't seem happy with the trait scope

  override def run(args: List[String]): Task[ExitCode] = new Day9(25).both.as(ExitCode.Success)
}

class Day9(preamble: Int) extends readDay9 {

  def both = {
    implicit val input: List[Long] = nums2

    (for {
      p1 <- part1
      p2 <- part2(p1)
    } yield p2).timed.map(println(_))
  }

  def part1(implicit input: List[Long]) = findInvalid.tapEval(p => Task(println(p)))

  def findInvalid(implicit input: List[Long]) = windowed(preamble + 1).filter(checkdata(2, _)).map(_.last).getLastValueAsTask

  def part2(p1: Long)(implicit input: List[Long]) =
    for {
      s <- findSum(p1, input).map(_._1.get)
    } yield (s.min + s.max)

  def windowed(n: Int)(implicit input: List[Long]) = Stream.emits(input).covary[Task].sliding(n).map(_.toList)

  def findData(groupSize: Int, nums: List[Long]): Option[List[Long]] = {
    val last = nums.last
    val sums = nums.init.combinations(groupSize).toList

    sums.find(s => s.sum == last)
  }

  def checkdata(groupSize: Int, nums: List[Long]): Boolean = findData(groupSize, nums).isEmpty

  def findSum(target: Long, nums: List[Long]) = {
    implicit val filteredNums = nums.filter(n => n <= target)
    val init: Option[List[Long]] = None

    TaskExt.iterate((init, 2))(_._1.isEmpty) {
      case (None, a) =>
        findSumForWindowSize(a + 1, target).getLastValueAsTask.onErrorFallbackTo(Task.pure(None)).map(o => (o, a + 1))
      case (Some(b), a) => Task {
        (Some(b), a)
      }
    }
  }

  def findSumForWindowSize(a: Int, target: Long)(implicit nums: List[Long]): Stream[Task, Option[List[Long]]] =
    windowed(a).map(w => findData(a, w :+ target)).filter(_.isDefined)
}

trait readDay9 extends FileUtils {
  def nums2 = scala.io.Source.fromFile("files/9.txt").getLines().map(_.toLong).toList
}
