/*
 * Copyright (c) 2020 James Fielder.
 * All rights reserved.
 */

package aoc.solutions

import aoc.utils.FileUtils
import cats.data.NonEmptyList
import cats.effect.ExitCode
import monix.eval.{Task, TaskApp}
import scala.util.chaining._
import fs2._

object DayThree extends TaskApp with FileUtils {

  case class Slope(right: Int, down: Int)

  val part1 = Seq(Slope(3, 1))

  val part2 = Seq(Slope(1, 1), Slope(3, 1), Slope(5, 1), Slope(7, 1), Slope(1, 2))

  val rows = readFileStream("files/3.txt").map(Row(_))

  override def run(args: List[String]): Task[ExitCode] = {
    agg(part1, rows).map {
      case out :: Nil => println("part 1 " + out)
    } *> agg(part2, rows).map { r =>
      println("part 2 " + r.reduce(_ * _))
    }.as(ExitCode.Success)
  }

  def agg(slopes: Seq[Slope], rows: Stream[Task, Row]): Task[Seq[Int]] =
    Task.sequence { slopes.map(program(_, rows)) }

  def program(s: Slope, rows: Stream[Task, Row]): Task[Int] = pathFinder(s, rows).pipe(treeFinder)

  def pathFinder(s: Slope, rows: Stream[Task, Row]): Stream[Task, (Row, Int)] =
    rows
      .chunkN(s.down, allowFewer = false)
      .map(_.head)
      .collect { case Some(i) => i }
      .zip(access(s))

  def treeFinder(rowStream: Stream[Task, (Row, Int)]): Task[Int] =
    rowStream.map { case (row, a) => row.get(a) }
      .filter(_ == Pos.Tree)
      .compile
      .toVector
      .map(_.size)

  def access(s: Slope): Stream[Pure, Int] = Stream(0) ++ Stream.iterate(s.right)(_ + s.right)

}

import enumeratum._

sealed abstract class Pos(override val entryName: String) extends EnumEntry
object Pos extends Enum[Pos] {
  case object Blank extends Pos(".")
  case object Tree extends Pos("#")

  val values: IndexedSeq[Pos] = findValues
}

case class Row(private val underlying: NonEmptyList[(Pos, Int)]) {
  def size: Int = underlying.size

  def get(index: Int): Pos = {
    val indexInRange = index % size
    underlying.find { case (_, i) => i == indexInRange }.get._1
  }
}
object Row {
  def apply(rowStr: String): Row =
    Row(
      NonEmptyList.fromListUnsafe(rowStr.toList.map(s => Pos.withName(s.toString))).zipWithIndex
    )
}
