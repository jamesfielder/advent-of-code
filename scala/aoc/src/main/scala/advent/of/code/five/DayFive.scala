/*
 * Copyright (c) 2020 James Fielder.
 * All rights reserved.
 */

package advent.of.code.five

import advent.of.code.utils.Utils
import cats.effect.ExitCode
import cats.implicits._
import monix.eval.{Task, TaskApp}
import fs2._

case class Seat(row: Int, column: Int) {
  def id: Int = (row * 8) + column
}

object DayFive extends TaskApp with Utils {
  override def run(args: List[String]): Task[ExitCode] =
    (part2 *> part1)
      .map(println(_)).as(ExitCode.Success)

  def part1: Task[Int] =
    parseBoardingCards
      .map(_.id)
      .compile.toList.map(_.max)

  def part2: Task[IndexedSeq[Unit]] = {
    seatIds.map(seats => {
      val min = seats.map(_._2).min
      val max = seats.map(_._2).max
      val range = min to max

      range.filter(r => !seats.exists { case (_, i) => i == r }).map(println(_))
    })
  }

  def seatIds: Task[List[(Seat, Int)]] =
    parseBoardingCards
      .map(s => (s, s.id))
      .compile.toList.map(_.sortBy(_._2))


  def parseBoardingCards: Stream[Task, Seat] =
    readFileStream("files/5.txt")
      .map(passToSeat)

  def passToSeat(boardingPass: String): Seat = Seat.tupled(
    boardingPass.splitAt(7).bimap(
      rs => Integer.parseInt(rs.replace("F", "0").replace("B", "1"), 2),
      cs => Integer.parseInt(cs.replace("L", "0").replace("R", "1"), 2)
    )
  )
}
