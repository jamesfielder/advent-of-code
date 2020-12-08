/*
 * Copyright (c) 2020 James Fielder.
 * All rights reserved.
 */

package aoc

import aoc.solutions.{Asm, DayEightParser, Instruction}

class AsmParserSpec extends munit.FunSuite {
  test("parse") {
    val i = "nop -465"

    assertEquals(DayEightParser.asmParser.parseAll(i), Right(Asm(Instruction.nop, -465)))
  }
}
