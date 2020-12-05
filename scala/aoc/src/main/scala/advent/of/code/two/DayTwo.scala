/*
 * Copyright (c) 2020 James Fielder.
 * All rights reserved.
 */

package advent.of.code.two

import advent.of.code.two.DayTwo.{Password, Rule}
import advent.of.code.utils.Utils
import cats.effect.ExitCode
import cats.parse.{Numbers, Parser, Parser1}
import monix.eval.{Task, TaskApp}
import fs2._

object DayTwo extends TaskApp with Utils {

  case class Rule(min: Int, max: Int, letter: Char)

  case class Password(rule: Rule, password: String)

  override def run(args: List[String]): Task[ExitCode] =
    (for {
      _ <- program(validatePasswordPart1, "part1")
      _ <- program(validatePasswordPart2, "part2")
    } yield ()).compile.drain.as(ExitCode.Success)

  val passwordParser: Stream[Task, Password] =
    readFileStream("files/2.txt")
      .map(parseLine)
      .collect { case Right(p) => p }

  def program(validator: Password => Boolean, name: String): Stream[Task, Unit] =
    passwordParser
      .filter(validator)
      .zipWithIndex
      .map(_._2 + 1) // +1 as zero indexed
      .last
      .map(o => println(name + " " + o))

  def parseLine(line: String): Either[Parser.Error, Password] = DayTwoParser.parser.parseAll(line)

  def validatePasswordPart1(p: Password): Boolean =
    p.password.toList
      .filter(_ == p.rule.letter)
      .groupBy(identity)
      .map { case (_, lists) => lists.size }
      .exists(count => (p.rule.min to p.rule.max).contains(count))

  def validatePasswordPart2(p: Password): Boolean =
    p.password.zipWithIndex.map { case (a, b) => (b + 1, a) }
      .filter(a => List(p.rule.max, p.rule.min).contains(a._1))
      .count { case (_, b) => b == p.rule.letter } == 1
}

object DayTwoParser {
  val dash: Parser1[Char] = Parser.charIn("-")
  val colon: Parser1[Char] = Parser.charIn(":")
  val space: Parser1[Char] = Parser.charIn(" ")
  val letter: Parser1[Char] = Parser.ignoreCaseCharIn('a' to 'z')

  def positiveDigit: Parser1[Int] = Numbers.nonNegativeIntString.map(_.toInt)

  // 10-11 f
  val rulesParser: Parser1[Rule] = (positiveDigit ~ (dash *> positiveDigit) ~ (space *> letter)).map {
    case ((min, max), letter) => Rule(min, max, letter)
  }

  // 10-11 f: fffffffffnqf
  val parser: Parser1[Password] = (rulesParser ~ (colon *> space *> letter.rep1.string)).map {
    case (rule, pass) => Password(rule, pass)
  }
}
