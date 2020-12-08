/*
 * Copyright (c) 2020 James Fielder.
 * All rights reserved.
 */

package aoc

import aoc.solutions.{B, Bag, BagRule}

class BagParserSpec extends munit.FunSuite {
  import solutions.DaySevenParser._

  test("parses a bag correctly") {
    val testBag = " light red bags contain 1 bright white bag, 2 muted yellow bags."

    assertEquals(
      bagParser.parseAll(testBag),
      Right(B(Bag("light red"), List(BagRule(Bag("bright white"), 1), BagRule(Bag("muted yellow"), 2))))
    )
  }

  test("parse another one") {
    val t = "wavy lavender bags contain 3 dotted cyan bags, 3 bright chartreuse bags, 5 plaid black bags, 3 dotted gold bags."

    assertEquals(
      bagParser.parseAll(t),
      Right(B(
        Bag("wavy lavender"),
        List(
          BagRule(Bag("dotted cyan"), 3),
          BagRule(Bag("bright chartreuse"), 3),
          BagRule(Bag("plaid black"), 5),
          BagRule(Bag("dotted gold"), 3)
        )
      ))
    )
  }

  test("parses an empty bagrule correctly") {
    val empty = "faded blue bags contain no other bags."

    assertEquals(
      bagParser.parseAll(empty),
      Right(B(Bag("faded blue"), List()))
    )
  }

  test("parse one bag rule") {
    val testRule1 = "1 shiny gold bag"

    assertEquals(bagRule.parseAll(testRule1), Right(BagRule(Bag("shiny gold"), 1)))
  }

  test("positive digit") {
    val pd = " 1"

    assertEquals(positiveDigit.parseAll(pd), Right(1))
  }
}
