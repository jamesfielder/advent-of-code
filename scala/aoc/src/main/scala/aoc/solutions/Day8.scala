/*
 * Copyright (c) 2020 James Fielder.
 * All rights reserved.
 */

package aoc.solutions

import aoc.utils.FileUtils
import cats.effect.ExitCode
import cats.parse.Numbers.nonNegativeIntString
import cats.parse.{Parser, Parser1}
import fs2._
import monix.eval.{Task, TaskApp}

object Day8 extends TaskApp with FileUtils {
  override def run(args: List[String]): Task[ExitCode] = part1.as(ExitCode.Success)

  def part1 =
    (for {
      a <- asm
      init = Recur(a.head, a, 0L, List.empty)
      out <- part1Stream(init)
    } yield out).map(println(_))

//  def part2 =
//    (for {
//      a <- asm
//
//    } yield ())
//
//  def part2Task(asm: List[(Asm, Long)]) = Task.delay {
//     Task.pure(Recur2(Recur(asm.head, asm, 0, List()), 0)).flatMapLoop((asm, 0))((a, b, c) => {
//
//     })
//  }

  def part1Stream(recur: Recur) =
    Stream.unfold(recur)(part1Interprieter)
      .covary[Task]
      .pull
      .last
      .flatMap(f => Pull.output1(f))
      .stream.compile.toList

  def part1Interprieter: Recur => Option[((Long, Instruction), Recur)] = (r: Recur) => {
    val op = r.current._1
    val index = r.current._2
    val instruction = r.current._1.instruction
    val alreadyVisited = r.seen.contains(index)
    val next = findInstruction(index + 1, r.program)
    val inc = r.current._1.i

    (alreadyVisited, op.instruction) match {
      case (true, _) => None
      case (_, Instruction.nop) => Some(((r.acc, instruction), Recur(next, r.program, r.acc, r.seen :+ index)))
      case (_, Instruction.acc) => Some(((r.acc, instruction), Recur(next, r.program, r.acc + inc, r.seen :+ index)))
      case (_, Instruction.jmp) => Some(((r.acc, instruction), Recur(findInstruction(index + inc, r.program), r.program, r.acc, r.seen :+ index)))
      case (_, Instruction.nxt) => None
    }
  }

  def swapInsAt(i: Long, ins: List[(Asm, Long)]): List[(Asm, Long)] = {
    val toSwap = findInstruction(i, ins)

    val swapped = toSwap match {
      case (Asm(Instruction.jmp, j), in) => (Asm(Instruction.nop, j), in)
      case (Asm(Instruction.nop, j), in) => (Asm(Instruction.jmp, j), in)
      case a => a
    }

    (ins.filter { case (_, l) => l < i } :+ swapped) ++ ins.filter { case (_, l) => l > i }
  }

  def findInstruction(j: Long, ins: List[(Asm, Long)]): (Asm, Long) =
    ins.find { case (_, i) => i == j }.get

  def nextNopOrJmp(j: Long, ins: List[(Asm, Long)]): Long =
    ins
      .filter { case (ins, i) => (i > j) && Seq(Instruction.jmp, Instruction.nop).contains(ins) }
      .head._2

  def addFinalIns(ins: List[(Asm, Long)]) = {
    val i = ins.map(_._2).max + 1
    ins :+ ((Asm(Instruction.nxt, 0), i))
  }

  def asm =
    readFileStream("files/8.txt")
      .map(DayEightParser.asmParser.parseAll)
      .collect { case Right(i) => i }
      .zipWithIndex.compile.toList
}

case class Recur(current: (Asm, Long), program: List[(Asm, Long)], acc: Long, seen: List[Long])

case class Recur2(r: Recur, lastChangedIndex: Long)

import enumeratum._

sealed trait Instruction extends EnumEntry

object Instruction extends Enum[Instruction] {

  case object acc extends Instruction

  case object jmp extends Instruction

  case object nop extends Instruction

  case object nxt extends Instruction

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