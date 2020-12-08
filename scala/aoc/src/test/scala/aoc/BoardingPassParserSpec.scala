/*
 * Copyright (c) 2020 James Fielder.
 * All rights reserved.
 */

package aoc

import solutions.{Day5, Seat}

class BoardingPassParserSpec extends munit.FunSuite {
  test("First example") {
    assertEquals(Day5.passToSeat("FBFBBFFRLR"), Seat(44, 5))
  }
}
