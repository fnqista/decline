package com.monovore.decline

import cats.syntax.all._
import com.monovore.decline._

object TestRun extends App {

  val a = Opts.option[Int]("a", help = "a").validate("a must be greater than zero")(_ > 0)
  val b = Opts.option[Int]("b", help = "b").validate("b must be greater than zero")(_ > 0)

  case class Config(a: Int, b: Int)

  val mainOptions = (a, b).mapN(Config.apply)

  Command("test", "test only!")(mainOptions)
    .parseLenient(
      Seq(
//      "--x=2"

        "--a=1"
        ,"--b=12"
        ,"--j=1"
        ,"--t"
        ,"1"
        ,"9"
      )
    ) match {
    case Left(help) if help.errors.isEmpty =>
      // help was requested by the user, i.e.: `--help`
      println(help)
      sys.exit(0)

    case Left(help) =>
      // user needs help due to bad/missing arguments
      System.err.println(help)
      sys.exit(1)

    case Right(parsedValue) =>
      Console println parsedValue._1
      Console println parsedValue._2.mkString(",")
  }

  Command("test", "test only!")(mainOptions)
    .parse(
      Seq(
        //      "--x=2"

        "--a=-11"
        ,"--b=-12"
        ,"--j=1"
        ,"--t"
        ,"1"
        ,"9"
      )
    ) match {
    case Left(help) if help.errors.isEmpty =>
      // help was requested by the user, i.e.: `--help`
      println(help)
      sys.exit(0)

    case Left(help) =>
      // user needs help due to bad/missing arguments
      System.err.println(help)
      sys.exit(1)

    case Right(parsedValue) =>
      Console println parsedValue
  }
}
