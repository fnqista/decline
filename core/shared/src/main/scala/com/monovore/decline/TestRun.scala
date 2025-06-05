package com.monovore.decline

import cats.syntax.all._
import com.monovore.decline._

object TestRun extends App {

  val a = Opts.option[Int]("aaa", help = "a", short = "a").validate("a must be greater than zero")(_ > 0)
  val b = Opts.option[Int]("bbb", help = "b", short = "b").validate("b must be greater than zero")(_ > 0)

  case class Config(a: Int, b: Int)

  val mainOptions = (a, b).mapN(Config.apply)

//  Command("test", "test only!")(mainOptions)
//    .parseLenient(
//      Seq(
//
//          "--j=3"
////        "--bbb=1"
////        ,"--bbb=12"
////        ,"--j=1"
////        , "--x=2"
////        ,"--aaa=12"
//
////        ,"-x"
////        ,"2"
////        ,"--j"
////        ,"--t"
////        ,"1"
////        ,"9"
//      )
//    ) match {
//    case Left(help) if help.errors.isEmpty =>
//      // help was requested by the user, i.e.: `--help`
//      println(help)
//      sys.exit(0)

//    case Left(help) =>
//      // user needs help due to bad/missing arguments
//      System.err.println(help)
//      sys.exit(1)
//
//    case Right(parsedValue) =>
//      Console println parsedValue._1
//      Console println parsedValue._2.mkString(",")
//  }

  Command("test", "test only!")(mainOptions)
    .parse(
      Seq(

        "--aaa=2"
//        ,"--aaa=-11"

        ,"-b"
        ,"12"

//        ,"--j=1"
//        ,"--t"
//        ,"1"
//        ,"9"
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
