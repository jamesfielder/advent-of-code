/*
 * Copyright (c) 2020 James Fielder.
 * All rights reserved.
 */

package aoc

import aoc.solutions.{B, Bag, BagRule}
import aoc.solutions.DaySeven._

class BagRuleCountSpec extends munit.FunSuite {
  test("one bag") {
    val b = B(Bag("1"), List())
    val bset = Set(b)

    assertEquals(bagRuleCount(b, bset), 0L)
  }

  test("one bag in bag") {
    val b = B(Bag("1"), List(BagRule(Bag("2"), 1)))
    val b2 = B(Bag("2"), List())
    val bset = Set(b, b2)

    assertEquals(bagRuleCount(b, bset), 1L)
  }

  test("two bag in bag") {
    val b = B(Bag("1"), List(BagRule(Bag("2"), 2)))
    val b2 = B(Bag("2"), List())
    val bset = Set(b, b2)

    assertEquals(bagRuleCount(b, bset), 2L)
  }

  test("two two in bag") {
    val b = B(Bag("1"), List(BagRule(Bag("2"), 2), BagRule(Bag("3"), 2)))
    val b2 = B(Bag("2"), List())
    val b3 = B(Bag("3"), List())
    val bset = Set(b, b2, b3)

    assertEquals(bagRuleCount(b, bset), 4L)
  }

  test("nesting simple") {
    val b = B(Bag("1"), List(BagRule(Bag("2"), 2))) // 2 + 2 * (4) = 2 + 8
    val b2 = B(Bag("2"), List(BagRule(Bag("3"), 2), BagRule(Bag("4"), 2))) //  2 + 2
    val b3 = B(Bag("3"), List())
    val b4 = B(Bag("4"), List())
    val bset = Set(b, b2, b3, b4)

    assertEquals(bagRuleCount(b, bset), 10L)
  }

  test("nesting less simple") {
    val b = B(Bag("1"), List(BagRule(Bag("2"), 2), BagRule(Bag("5"), 2))) // (2 + 2 * (14)) + (2 + 2 * (1)) = 2 + 28 + 2 + 2 = 28 + 6 = 34
    val b2 = B(Bag("2"), List(BagRule(Bag("3"), 2), BagRule(Bag("4"), 2))) // (2 + 2 * (5)) + (2 + 2 * (0)) = 14
    val b3 = B(Bag("3"), List(BagRule(Bag("4"), 5))) // 5
    val b4 = B(Bag("4"), List()) // 0
    val b5 = B(Bag("5"), List(BagRule(Bag("6"), 1))) // 6[1] = 1
    val b6 = B(Bag("6"), List()) // 0
    val bset = Set(b, b2, b3, b4, b5, b6)

    assertEquals(bagRuleCount(b, bset), 34L)
  }

  test("mixed nesting levels") {
    val b = B(Bag("1"), List(BagRule(Bag("2"), 2), BagRule(Bag("5"), 2))) // 2 + 2 * (12) + 2 == 2 + 24 + 2 = 28
    val b2 = B(Bag("2"), List(BagRule(Bag("3"), 2))) // 2 + (2 * 5) = 12
    val b3 = B(Bag("3"), List(BagRule(Bag("4"), 5))) // 5
    val b4 = B(Bag("4"), List()) // 0
    val b5 = B(Bag("5"), List()) // 6[1] = 1
    val bset = Set(b, b2, b3, b4, b5)

    assertEquals(bagRuleCount(b, bset), 28L)
  }
}