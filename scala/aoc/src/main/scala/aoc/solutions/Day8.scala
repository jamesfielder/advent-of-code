/*
 * Copyright (c) 2020 James Fielder.
 * All rights reserved.
 */

package aoc.solutions

import aoc.utils.FileUtils
import cats.effect.ExitCode
import cats.parse.Numbers.nonNegativeIntString
import cats.parse.{Parser, Parser1}
import monix.eval.{Task, TaskApp}

object Day8 extends TaskApp with FileUtils {
  override def run(args: List[String]): Task[ExitCode] = part1.as(ExitCode.Success)

  def part1 =
    asm.zipWithIndex.debug().compile.drain

  def asm =
    readFileStream("files/8.txt")
      .map(DayEightParser.asmParser.parseAll)
      .collect { case Right(i) => i }
}

import enumeratum._

sealed trait Instruction extends EnumEntry
object Instruction extends Enum[Instruction] {
  case object acc extends Instruction
  case object jmp extends Instruction
  case object nop extends Instruction

  override def values: IndexedSeq[Instruction] = findValues
}

case class Asm(instruction: Instruction, i: Int)

object DayEightParser {
  import aoc.utils.ParserExtras._

  val insP: Parser1[Instruction] = word.map(Instruction.withName)

  val signedIntString: Parser1[String] =
    (Parser.charIn("+-").?.with1 ~ nonNegativeIntString).string

  // nop -465
  val asmParser = (insP ~ signedIntString).map {
    case (ins, i) => Asm(ins, i.toInt)
  }
}