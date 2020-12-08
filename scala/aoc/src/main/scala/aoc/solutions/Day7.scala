/*
 * Copyright (c) 2020 James Fielder.
 * All rights reserved.
 */

package aoc.solutions

import aoc.utils.FileUtils
import cats.effect.ExitCode
import cats.parse.{Parser, Parser1}
import monix.eval.{Task, TaskApp}

object Day7 extends TaskApp with FileUtils {
  override def run(args: List[String]): Task[ExitCode] = (part1 *> part2).as(ExitCode.Success)

  def part1 =
    readBags.compile.toList
      .map(r => r.map(bag => (bag.bag, bToSubBags(bag, r))))
      .map(_.filter(_._2.contains(Bag("shiny gold"))))
      .map(l => println(l.size))

  def part2 =
    readBags.compile.toList
      .map(r => r.map(bag => (bag, bToSubSet(bag, r, Set[B]()))))
      .map(_.filter(_._1.bag == Bag("shiny gold")))
      .map(_.map(s => bagRuleCount(s._1, s._2)))
      .map(println(_))

  def bagRuleCount(b: B, bags: Set[B]): Long = {
    b.bagRule.foldLeft(0L)((acc, br) => {
      val fb = findFullBag(br.bag, bags.toList)

      fb.bagRule match {
        case Nil => acc + br.amount
        case _ => acc + (br.amount + (br.amount * bagRuleCount(fb, bags)))
      }
    })
  }

  def bToSubBags(b: B, bags: List[B]): Set[Bag] =
    bToSubSet(b, bags, Set[B]()).map(_.bag)

  def bToSubSet(b: B, bags: List[B], found: Set[B]): Set[B] = {
    val containing = b.bagRule.map(_.bag)
    val cBags = containing.map(findFullBag(_, bags)).toSet
    val newFound = found.concat(cBags)

    b.bagRule match {
      case Nil => newFound.concat(Set(b))
      case _ => cBags.flatMap(bToSubSet(_, bags, newFound))
    }
  }

  def findFullBag(bag: Bag, bags: List[B]): B = {
    bags.find(_.bag == bag).get
  }

  def readBags =
    readFileStream("files/7.txt")
      .map(DaySevenParser.bagParser.parseAll)
      .collect { case Right(p) => p }
}

case class BagRule(bag: Bag, amount: Int)

case class Bag(name: String)

case class B(bag: Bag, bagRule: List[BagRule])

object DaySevenParser {

  import aoc.utils.ParserExtras._

  val delim = Parser.charIn(",.").surroundedBy(whitespaces0).void
  val bagsStr = Parser.string1("bags").surroundedBy(whitespaces0).void
  val bagStr = Parser.string1("bag").surroundedBy(whitespaces0).void
  val contain = Parser.string1("contain").surroundedBy(whitespaces0).void
  val noBags = Parser.string1("no other bags.").as(List[BagRule]())
  val bagz = bagsStr.orElse(bagStr).void

  val bag = (word ~ word).map {
    case (w1, w2) => s"$w1 $w2"
  }

  val bagRule: Parser1[BagRule] = (positiveDigit ~ bag <* bagz).map {
    case (d, b) => BagRule(Bag(b), d)
  }

  val rules =
    (bagRule <* delim).rep1.map(_.toList).backtrack.orElse1(noBags)

  val first = (bag <* bagsStr) <* contain

  val bagParser = (first ~ rules).map {
    case (bag, bagRule) => B(Bag(bag), bagRule)
  }

}
