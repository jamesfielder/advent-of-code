/*
 * Copyright (c) 2020 James Fielder.
 * All rights reserved.
 */

package aoc.solutions

import scala.io.Source
import cats.implicits._

object Day10 extends App {

  def read10 = Source.fromFile("files/10.txt").getLines().map(_.toInt).toList

  val smallTestCase = List(16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4)
  val testCase = List(28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19, 38, 39, 11, 1, 32, 25, 35, 8, 17, 7, 9, 4, 2, 34, 10, 3)

  val input = read10

  val deviceRating = input.max + 3

  val effective = 0 +: input.sorted :+ deviceRating

  val diffs = effective.sliding(2).map {
    case x :: y :: Nil => y - x
    case _ => throw new Exception("Cannot find adapter")
  }.toList

  val count = diffs.partition(_ == 1).bimap(
    _.size, _.size
  )

  println(effective)
  println(splitAtDiff3(effective))
  println(splitAtDiff3(effective).map(countPossibleArrangements).product)
  println(diffs)
  println(count._1 * count._2)

  def splitAtDiff3(nums: List[Int]): List[List[Int]] = {

    def addToFinalList(a: Int, nums: List[List[Int]]): List[List[Int]] =
      nums match {
        case l :: Nil => List(l :+ a)
        case l :: ls => l :: addToFinalList(a, ls)
        case Nil => throw new Exception("Should never happen because the list is initialised with an element")
      }

    nums.foldLeft(List[List[Int]](List(0)))((acc, a) => {
      val last = acc.last.last

      if (a == last + 1) addToFinalList(a, acc) else addToFinalList(a, acc :+ List())
    })
  }

  def countPossibleArrangements(n: List[Int]): Long =
    n.size match {
      case 1 => 1L
      case 2 => 1L
      case 3 => 2L
      case 4 => 4L
      case 5 => 7L
      case _ => ???
    }

  //def permuateListAllowed
}
