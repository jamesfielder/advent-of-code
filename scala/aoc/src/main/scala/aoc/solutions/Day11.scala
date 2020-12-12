/*
 * Copyright (c) 2020 James Fielder.
 * All rights reserved.
 */

package aoc.solutions

import cats._
import cats.implicits._
import cats.kernel.Eq
import io.scalaland.catnip._

import scala.None
import scala.util.Try
//import aoc.utils.Stuff._
import scala.io.Source

object Day11 extends App {
  import Data._
  val input = Source.fromFile("files/11.txt").getLines().toList

  val data = TestData.small
//  val data = input
  val start = parseInput(data)
//  println(start)

//  val p1 = part1
  val p2 = part2
//  println(p1.countOccupied)
  println(p2.countOccupied)

  def part1: Ferry = iterate(changeSeatPart1, start)

  def part2: Ferry = iterate(changeSeatPart2, start)

  def iterate(stepFn: Ferry => Seat => Seat, start: Ferry): Ferry =
    Iterator
      .iterate(Recur(start, start.mapSeats(stepFn.apply(start)))) { r =>
        println(r.next)
        Recur(r.next, r.next.mapSeats(stepFn.apply(r.next)))
      }
      .find(f => f.last === f.next)
      .get
      .last

  def changeSeatPart1: Ferry => Seat => Seat =
    (f: Ferry) =>
      (seat: Seat) => {
        val candidatePoints =
          generateNeighbours(seat.asPoint)
            .filter(n => n._1 < f.width && n._2 < f.length)
            .filter(c => seat.asPoint != c) // Don't count yourself!!

        val candidateSeats = candidatePoints.map(p => f.getSeatAtPoint(p))

        val candidates = candidateSeats.count(_.state == State.Taken)

        applySeatRules(seat, candidates, 4)
      }

  def changeSeatPart2: Ferry => Seat => Seat =
    (f: Ferry) =>
      (seat: Seat) => {
        val deltas = Seq((0, 1), (0, -1), (1, 0), (-1, 0), (1, 1), (-1, 1), (1, -1), (-1, -1))

        if (seat.state == State.Floor) seat
        else {
          val candidateSeats = deltas.map(findFirstSeatInDirection(seat, _, f)).collect { case Some(s) => s }
          val candidates = candidateSeats.count(_.state == State.Taken)

          applySeatRules(seat, candidates, 5)
        }
      }

  def findFirstSeatInDirection(seat: Seat, delta: (Int, Int), f: Ferry): Option[Seat] =
    Iterator
      .iterate(Option.apply(seat)) {
        case Some(seat) => Try(nextPoint(seat, delta, f)).toOption
        case None => None
      }
      .find(os =>
        os match {
          case Some(s) => s.asPoint != seat.asPoint && s.state != State.Floor
          case None => true
        }
      )
      .flatten

  def nextPoint(s: Seat, delta: (Int, Int), f: Ferry): Seat =
    f.getSeatAtPoint(
      s.asPoint.bimap(x => x + delta._1, y => y + delta._2)
    )

  def applySeatRules(seat: Seat, candidates: Int, tolerance: Int): Seat =
    (seat.state, candidates) match {
      case (State.Empty, x) if x == 0 => seat.copy(state = State.Taken)
      case (State.Taken, x) if x >= tolerance => seat.copy(state = State.Empty)
      case _ => seat
    }

  def parseInput(lines: List[String]): Ferry =
    Ferry(lines.zipWithIndex.foldLeft(Vector[Seat]())((acc, s) => {
      acc ++ s._1.toList.zipWithIndex.map(seat => Seat(seat._2, s._2, State.withName(seat._1.toString)))
    }))

  def generateNeighbours(point: (Int, Int)): Seq[(Int, Int)] = {
    val (x, y) = (point._1, point._2)

    cartesianProduct(ranges(x), ranges(y))
  }

  def ranges(x: Int): Seq[Int] = x - 1 to x + 1 filter (_ >= 0)

  def cartesianProduct[P, Q](a: Seq[P], b: Seq[Q]): Seq[(P, Q)] = {
    for {
      as <- a
      bs <- b
    } yield (as, bs)
  }
}

object Data {

  import enumeratum._
  sealed abstract class State(override val entryName: String) extends EnumEntry {
    override def toString: String = entryName
  }
  object State extends Enum[State] with CatsEnum[State] {
    case object Floor extends State(".")
    case object Empty extends State("L")
    case object Taken extends State("#")

    override def values: IndexedSeq[State] = findValues
  }

  @Semi(Eq) case class Recur(last: Ferry, next: Ferry)

  @Semi(Eq, Show, Hash) case class Seat(x: Int, y: Int, state: State) {
    def asPoint: (Int, Int) = (x, y)
  }

  @Semi(Eq, Show, Hash) case class Ferry(seats: Vector[Seat]) {
    val width = seats.maxBy(_.x).x + 1
    val length = seats.maxBy(_.y).y + 1

    def getSeatAtPoint(p: (Int, Int)) = {
      val index = (p._2 * width) + p._1
      seats(index)
    }

    override def toString: String = {
      seats
        .sliding(width, width)
        .map(_.map(_.state).mkString + System.lineSeparator)
        .mkString
    }

    def countOccupied: Int = seats.count(_.state == State.Taken)

    def mapSeats(f: Seat => Seat): Ferry = Ferry(this.seats.map(f))
  }
}

object TestData {
  val small =
    """L.LL.LL.LL
      |LLLLLLL.LL
      |L.L.L..L..
      |LLLL.LL.LL
      |L.LL.LL.LL
      |L.LLLLL.LL
      |..L.L.....
      |LLLLLLLLLL
      |L.LLLLLL.L
      |L.LLLLL.LL
      |""".stripMargin
      .split(System.lineSeparator)
      .toList

  val roundOne =
    """#.##.##.##
      |#######.##
      |#.#.#..#..
      |####.##.##
      |#.##.##.##
      |#.#####.##
      |..#.#.....
      |##########
      |#.######.#
      |#.#####.##
      |""".stripMargin
      .split(System.lineSeparator)
      .toList

  val roundTwo =
    """#.LL.L#.##
      |#LLLLLL.L#
      |L.L.L..L..
      |#LLL.LL.L#
      |#.LL.LL.LL
      |#.LLLL#.##
      |..L.L.....
      |#LLLLLLLL#
      |#.LLLLLL.L
      |#.#LLLL.##""".stripMargin
      .split(System.lineSeparator)
      .toList

  val roundTwoPart2 =
    """#.LL.LL.L#
      |#LLLLLL.LL
      |L.L.L..L..
      |LLLL.LL.LL
      |L.LL.LL.LL
      |L.LLLLL.LL
      |..L.L.....
      |LLLLLLLLL#
      |#.LLLLLL.L
      |#.LLLLL.L#""".stripMargin
      .split(System.lineSeparator)
      .toList
}
