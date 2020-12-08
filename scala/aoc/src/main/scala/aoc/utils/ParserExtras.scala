/*
 * Copyright (c) 2020 James Fielder.
 * All rights reserved.
 */

package aoc.utils

import cats.parse.{Numbers, Parser, Parser1}

object ParserExtras {

  val whitespace: Parser1[Unit] = Parser.charIn(" \t\r\n").void
  val whitespaces0: Parser[Unit] = whitespace.rep.void
  val word = Parser.charIn('a' to 'z').rep1.surroundedBy(whitespaces0).map(_.toList.mkString)

  def positiveDigit: Parser1[Int] = Numbers.nonNegativeIntString.surroundedBy(whitespaces0).map(_.toInt)

  /** These are taken from the cats parse repo, they're just not released yet! */
  implicit class ParserExtras[+A](p: Parser[A]) {
    /** Use this parser to parse between values.
     *
     * Parses `b` followed by `this` and `c`.
     * Returns only the values extracted by `this` parser.
     */
    def between(b: Parser[Any], c: Parser[Any]): Parser[A] =
      (b.void ~ (p ~ c.void)).map { case (_, (a, _)) => a }

    /** Use this parser to parse surrounded by values.
     *
     * This is the same as `between(b, b)`
     */
    def surroundedBy(b: Parser[Any]): Parser[A] =
      between(b, b)
  }

  implicit class Parser1Extras[+A](p1: Parser1[A]) {
    def between(b: Parser[Any], c: Parser[Any]): Parser1[A] =
      (b.void.with1 ~ (p1 ~ c.void)).map { case (_, (a, _)) => a }

    def surroundedBy(b: Parser[Any]): Parser1[A] =
      between(b, b)
  }
}
