/*
 * Copyright (c) 2020 James Fielder.
 * All rights reserved.
 */

package aoc

import aoc.solutions.{Pos, Row}
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Gen.nonEmptyListOf
import org.scalacheck.Prop._

class RowSpec extends munit.FunSuite {

  val line1 = ".....#...#.#.#....##.#......#.#"

  test("Row throws when constructing with empty line") {
    intercept[IllegalArgumentException]{
      Row("")
    }
  }

  test("Row is correctly parsed") {
    val row = Row(line1)

    assertEquals(row.size, 31)
    assertEquals(row.get(0), Pos.Blank)
    assertEquals(row.get(30), Pos.Tree)
  }

  test("Other characters fail") {
    val line = "whatever"
    intercept[NoSuchElementException] {
      Row(line)
    }
  }

  test("Wrap around works as expected") {
    val row = Row(line1)
    assertEquals(row.get(31), Pos.Blank)

    val size = row.size
    assertEquals(row.get(size * 3), row.get(size))
  }
}

class RowProperties extends ScalaCheckSuite {

  val lineGen: Gen[String] = Gen.oneOf("#", ".")
  val rowString: Gen[String] = nonEmptyListOf(lineGen).map(_.mkString)
  val accessor: Gen[Int] = Gen.choose(1, 100000)

  property("Wrap around works correctly") {
    forAll(rowString, accessor) { (row: String, num: Int) =>
      val r = Row(row)
      val size = r.size
      val min = num % size

      assertEquals(r.get(num), r.get(min))
    }
  }
}
