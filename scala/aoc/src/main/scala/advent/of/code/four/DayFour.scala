/*
 * Copyright (c) 2020 James Fielder.
 * All rights reserved.
 */

package advent.of.code.four

import advent.of.code.utils.Utils
import cats.effect.ExitCode
import monix.eval.{Task, TaskApp}

object DayFour extends TaskApp with Utils {

  import Validators._

  val required = Seq("ecl", "pid", "iyr", "eyr", "hcl", "byr", "hgt")

  override def run(args: List[String]): Task[ExitCode] =
    (for {
      _ <- program(part1Validator, "part 1")
      _ <- program(validatePassport, "part 2")
    } yield ()).as(ExitCode.Success)

  def program(validator: Map[String, String] => Boolean, message: String): Task[Unit] =
    lines.filter(validator)
      .compile.toList.map(p => println(message + ": " + p.size))

  def part1Validator(passport: Map[String, String]): Boolean =
    required.forall(passport.keys.toList.contains(_))

  private def lines = readFileStream("files/4.txt")
    .split(_.isBlank)
    .map(_.foldLeft("")((a, b) => a + " " + b))
    .map(chunkToMap)

  private def chunkToMap(c: String): Map[String, String] = {
    c.trim.split(" ").toList.map { s =>
      s.split(":").toList match {
        case k :: v :: Nil => k -> v
        case _ => throw new IllegalArgumentException(s"String $s was not parsable for this challenge")
      }
    }.toMap
  }
}

object Validators {

  type Validator = String => Boolean

  def validatePassport(passport: Map[String, String]): Boolean = {
    val filtered = passport.filter(p => part2Validator.keys.toList.contains(p._1))

    filtered.keys.size == 7 && filtered.forall { case (k, v) => part2Validator(k).apply(v) }
  }

  val part2Validator = Map[String, Validator](
    "byr" -> fieldBetween(1920, 2002),
    "iyr" -> fieldBetween(2010, 2020),
    "eyr" -> fieldBetween(2020, 2030),
    "hgt" -> allowedHeight,
    "hcl" -> allowedHairColour,
    "ecl" -> allowedEyeColour,
    "pid" -> allowedPassportId
  )

  def fieldBetween(lower: Int, upper: Int): Validator = (s: String) => s.toInt.inside(lower, upper)

  def allowedHeight: Validator = {
    case s"${num}cm" => fieldBetween(150, 193).apply(num)
    case s"${num}in" => fieldBetween(59, 76).apply(num)
    case _ => false
  }

  def allowedHairColour: Validator = {
    case s"#${hc}" => hc.length == 6 && hc.allowedChars
    case _ => false
  }

  def allowedEyeColour: Validator = (e: String) => Seq("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(e)

  def allowedPassportId: Validator = (p: String) => p.length == 9 && p.allowedNumbersOnly

  implicit class RangeValidator(i: Int) {
    def inside(lower: Int, upper: Int): Boolean = i >= lower && i <= upper
  }

  implicit class CharacterValidator(s: String) {
    val allowed: Seq[Char] = ('a' to 'f') ++ ('0' to '9')
    val numbers: Seq[Char] = '0' to '9'

    def allowedChars: Boolean = s.toList.forall(allowed.contains(_))

    def allowedNumbersOnly: Boolean = s.toList.forall(numbers.contains(_))
  }

}
