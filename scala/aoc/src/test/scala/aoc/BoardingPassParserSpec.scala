/*
 * Copyright (c) 2020 James Fielder.
 * All rights reserved.
 */

package aoc

import solutions.{DayFive, Seat}

class BoardingPassParserSpec extends munit.FunSuite {
  test("First example") {
    assertEquals(DayFive.passToSeat("FBFBBFFRLR"), Seat(44, 5))
  }
}
