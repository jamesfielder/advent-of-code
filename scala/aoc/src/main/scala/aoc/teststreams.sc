import fs2._
import aoc.utils.FileUtils
import cats.effect.ExitCode
import monix.eval.{Task, TaskApp}

import scala.collection.View.Empty.toMap

Stream(1, 2, 3).chunkN(2, allowFewer = false).take(2).map(_.last).collect{ case Some(i) => i}.toList

object Tester extends TaskApp with FileUtils {
  override def run(args: List[String]): Task[ExitCode] = ???

  def readFour = readFileStream("C:\\Users\\jcfie\\code\\advent-of-code\\scala\\files\\4.txt")
}

import monix.execution.Scheduler.Implicits.global

val test = ("eyr:2039".split(":").toList match {
  case k :: v :: Nil => k -> v
})

Map[String, String]() + ("a" -> "b")


Tester.readFour
  .split(_.isBlank)
//  .debug()
//  .evalTap(o => Task { println(o.foldLeft("")((a, b) => a + b)) })
//  .map(cs => cs.foldLeft(Map[String, String]()){ (m, s) =>
//    val split: (String, String) = s.split(":").toList match { case k :: v :: Nil => k -> v}
//    m + split
//  })

  .compile.toList.runSyncUnsafe()
  .foreach(println(_))
//  println("++++")
//  println(l)
//})