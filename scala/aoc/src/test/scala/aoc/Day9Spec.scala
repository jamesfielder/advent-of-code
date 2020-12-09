/*
 * Copyright (c) 2020 James Fielder.
 * All rights reserved.
 */

package aoc

import aoc.solutions.Day9
import monix.execution.Scheduler.Implicits.global

class Day9Spec extends munit.FunSuite {

  implicit val testData = List[Long](
    35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102, 117, 150, 182, 127, 219, 299, 277, 309, 576
  )

  val testDay9 = new Day9(5)

  test("part1 example") {
    assertEquals(testDay9.part1.runSyncUnsafe(), 127L)
  }

  test("part2 example") {
    testDay9.findSum(127L, testData).map(println(_)).runSyncUnsafe()
//    assertEquals(testDay9.findSum(127L, testData), )
  }
}

object TestOutput {

}
