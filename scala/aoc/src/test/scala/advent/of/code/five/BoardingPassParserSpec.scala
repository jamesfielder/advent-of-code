/*
 * Copyright (c) 2020 James Fielder.
 * All rights reserved.
 */

package advent.of.code.five

class BoardingPassParserSpec extends munit.FunSuite {
  test("First example") {
    assertEquals(DayFive.passToSeat("FBFBBFFRLR"), Seat(44, 5))
  }
}
