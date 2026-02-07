package com.monovore.decline

import cats.data.Ior
import cats.syntax.all._

object TestRun extends App {
  val a =
    Opts.option[Int]("aaa", help = "a", short = "a").validate("a must be greater than zero")(_ > 0)
  val b =
    Opts.option[Int]("bbb", help = "b", short = "b").validate("b must be greater than zero")(_ > 0)

  case class Config(a: Int, b: Int)

  val mainOptions = (a, b).mapN(Config.apply)
  val command = Command("test", "test only!")(mainOptions)

  // Test inputs with unexpected options and args
  val argInputsWithUnexpected: Seq[String] = Seq(
    "-a", "2",
    "--bbb=3",
    "--unexpected-option",
    "unexpected-arg"
  )


  // Test inputs with valid options only
  val argInputsValid: Seq[String] = Seq(
    "-a", "2",
    "--bbb=3"
  )

  // Test inputs with validation errors
  val argInputsValidationError: Seq[String] = Seq(
    "-a", "-2",  // negative, will fail validation
    "--bbb=3"
  )

  println("=" * 60)
  println("TEST 1: Strict mode with unexpected options/args")
  println("=" * 60)
  command.parse(argInputsWithUnexpected) match {
    case Left(help) if help.errors.isEmpty =>
      println("Help was requested by the user")
      println(help)
    case Left(help) =>
      println("STRICT MODE: Parsing failed (expected)")
      println(help)
    case Right(parsedValue) =>
      println(s"STRICT MODE: Parsed value: $parsedValue")
  }

  println()
  println("=" * 60)
  println("TEST 2: Lenient mode with unexpected options/args")
  println("=" * 60)
  command.parseLenient(argInputsWithUnexpected) match {
    case Ior.Left(help) =>
      println("LENIENT MODE: Parsing failed with errors only")
      println(help)
    case Ior.Right(parsedValue) =>
      println(s"LENIENT MODE: Parsed value with no warnings: $parsedValue")
    case Ior.Both(help, parsedValue) =>
      println(s"LENIENT MODE: Parsed value WITH warnings: $parsedValue")
      println(help)
  }

  println()
  println("=" * 60)
  println("TEST 3: Lenient mode with valid options only")
  println("=" * 60)
  command.parseLenient(argInputsValid) match {
    case Ior.Left(help) =>
      println("LENIENT MODE: Parsing failed with errors only")
      println(help)
    case Ior.Right(parsedValue) =>
      println(s"LENIENT MODE: Parsed value with no warnings: $parsedValue")
    case Ior.Both(help, parsedValue) =>
      println(s"LENIENT MODE: Parsed value WITH warnings: $parsedValue")
      println(help)
  }

  println()
  println("=" * 60)
  println("TEST 4: Lenient mode with validation errors")
  println("=" * 60)
  command.parseLenient(argInputsValidationError) match {
    case Ior.Left(help) =>
      println("LENIENT MODE: Parsing failed with validation errors (expected)")
      println(help)
    case Ior.Right(parsedValue) =>
      println(s"LENIENT MODE: Parsed value with no warnings: $parsedValue")
    case Ior.Both(help, parsedValue) =>
      println(s"LENIENT MODE: Parsed value WITH warnings: $parsedValue")
      println(help)
  }

  println()
  println("=" * 60)
  println("TEST 5: Lenient mode with validation errors AND unexpected args")
  println("=" * 60)
  val argInputsValidationErrorAndUnexpected: Seq[String] = Seq(
    "-a", "-2",  // negative, will fail validation
    "--bbb=3",
    "--unexpected",
    "unexpected-arg"
  )
  command.parseLenient(argInputsValidationErrorAndUnexpected) match {
    case Ior.Left(help) =>
      println("LENIENT MODE: Parsing failed - warnings promoted to errors (expected)")
      println(help)
    case Ior.Right(parsedValue) =>
      println(s"LENIENT MODE: Parsed value with no warnings: $parsedValue")
    case Ior.Both(help, parsedValue) =>
      println(s"LENIENT MODE: Parsed value WITH warnings: $parsedValue")
      println(help)
  }

  // Test 6: Parse error (missing value for option) should fail even in lenient mode
  println()
  println("=" * 60)
  println("TEST 6: Lenient mode with missing value for option (parse error)")
  println("=" * 60)
  val argInputsMissingValue: Seq[String] = Seq(
    "-a", "2",
    "--bbb"  // missing value!
  )
  command.parseLenient(argInputsMissingValue) match {
    case Ior.Left(help) =>
      println("LENIENT MODE: Parsing failed due to parse error (expected)")
      println(help)
    case Ior.Right(parsedValue) =>
      println(s"LENIENT MODE: Parsed value with no warnings: $parsedValue (UNEXPECTED!)")
    case Ior.Both(help, parsedValue) =>
      println(s"LENIENT MODE: Parsed value WITH warnings: $parsedValue (UNEXPECTED!)")
      println(help)
  }

  // Test 7: Parse error + unexpected should show both as errors
  println()
  println("=" * 60)
  println("TEST 7: Lenient mode with parse error AND unexpected args")
  println("=" * 60)
  val argInputsParseErrorAndUnexpected: Seq[String] = Seq(
    "-a", "2",
    "--bbb",  // missing value - parse error
    "--unexpected",
    "unexpected-arg"
  )
  command.parseLenient(argInputsParseErrorAndUnexpected) match {
    case Ior.Left(help) =>
      println("LENIENT MODE: Parsing failed - parse error makes everything fail (expected)")
      println(help)
    case Ior.Right(parsedValue) =>
      println(s"LENIENT MODE: Parsed value with no warnings: $parsedValue (UNEXPECTED!)")
    case Ior.Both(help, parsedValue) =>
      println(s"LENIENT MODE: Parsed value WITH warnings: $parsedValue (UNEXPECTED!)")
      println(help)
  }
}
