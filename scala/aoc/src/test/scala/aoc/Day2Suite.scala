/*
 * Copyright (c) 2020 James Fielder.
 * All rights reserved.
 */

package aoc

import solutions.Day2._
import solutions.{Day2, DayTwoParser}
import utils.FileUtils

class Day2Suite extends munit.FunSuite with FileUtils {

  test("Parse a rule") {
    val rule = "10-11 f"
    val expected = Right(Rule(10, 11, 'f'))

    assertEquals(DayTwoParser.rulesParser.parseAll(rule), expected)
  }

  test("Parse one line") {
    val line = "1-3 k: kskk"
    val expected = Right(Password(Rule(1, 3, 'k'), "kskk"))
    assertEquals(Day2.parseLine(line), expected)
  }

  test("Parse another line") {
    val line = "2-3 f: nvffq"
    val expected = Right(Password(Rule(2, 3, 'f'), "nvffq"))

    assertEquals(Day2.parseLine(line), expected)
  }

  test("Validate Password") {
    val pass = Password(Rule(2, 3, 'f'), "nvffq")
    assertEquals(validatePasswordPart1(pass), true)
  }

  test("Validate Password fail lower") {
    val pass = Password(Rule(2, 3, 'f'), "nvfq")
    assertEquals(validatePasswordPart1(pass), false)
  }

  test("Validate Password fail upper") {
    val pass = Password(Rule(2, 3, 'f'), "nvffffffffffq")
    assertEquals(validatePasswordPart1(pass), false)
  }

  test("Part 2 testing") {
    val line = "1-3 a: abcde"
    val password = Day2.parseLine(line).getOrElse(throw new Exception())

    assertEquals(validatePasswordPart2(password), true)
  }

  test("Part 2 another") {
    val line = "1-3 b: cdefg"
    val password = Day2.parseLine(line).getOrElse(throw new Exception())

    assertEquals(validatePasswordPart2(password), false)
  }

  test("Part 2 another another") {
    val line = "2-9 c: ccccccccc"
    val password = Day2.parseLine(line).getOrElse(throw new Exception())

    assertEquals(validatePasswordPart2(password), false)
  }
}
