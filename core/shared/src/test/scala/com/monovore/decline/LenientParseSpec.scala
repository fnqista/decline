package com.monovore.decline

import cats.data.{Ior, NonEmptyList}
import cats.syntax.all._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class LenientParseSpec extends AnyWordSpec with Matchers {

  def isMissing(msg: String): Boolean = msg.startsWith("Missing expected")

  implicit class LenientParser[A](opts: Opts[A]) {
    val command: Command[A] = Command("test-spec", header = "Test command!", helpFlag = false)(opts)

    def parseLenient(args: Seq[String], env: Map[String, String] = Map()): Ior[Help, A] =
      command.parseLenient(args, env)

    def parseStrict(args: Seq[String], env: Map[String, String] = Map()): Either[Help, A] =
      command.parse(args, env)
  }

  "Lenient Parsing" should {

    val whatever = Opts.option[String]("whatever", help = "Useful!")
    val ghost = Opts.option[String]("ghost", short = "g", help = "Important!")
    val positional = Opts.argument[String]("expected")
    val flag = Opts.flag("test", help = "...", short = "t")

    "succeed with no warnings when input is valid" in {
      val Ior.Right(result) = whatever.parseLenient(List("--whatever", "man"))
      result should equal("man")
    }

    "read a single option with unexpected trailing option" in {
      whatever.parseLenient(List("--whatever", "man", "--unknown")) match {
        case Ior.Both(help, result) =>
          result should equal("man")
          help.warnings should contain(Messages.unexpectedOption("--unknown"))
        case other => fail(s"Expected Ior.Both, got $other")
      }
    }

    "read a long option with = and unexpected option with value" in {
      whatever.parseLenient(List("--whatever=man", "--unknown=value")) match {
        case Ior.Both(help, result) =>
          result should equal("man")
          help.warnings should contain(Messages.unexpectedOption("--unknown"))
        case other => fail(s"Expected Ior.Both, got $other")
      }
    }

    "read a couple options with unexpected short options" in {
      val opts = (whatever, ghost).tupled
      opts.parseLenient(List("--whatever", "man", "-x", "--ghost", "dad")) match {
        case Ior.Both(help, result) =>
          result should equal(("man", "dad"))
          help.warnings should contain(Messages.unexpectedOption("-x"))
        case other => fail(s"Expected Ior.Both, got $other")
      }
    }

    "read options with multiple unexpected short options bundled" in {
      val opts = (whatever, ghost).tupled
      opts.parseLenient(List("--whatever", "man", "-xyz", "--ghost", "dad")) match {
        case Ior.Both(help, result) =>
          result should equal(("man", "dad"))
          help.warnings should contain(Messages.unexpectedOption("-x"))
          help.warnings should contain(Messages.unexpectedOption("-y"))
          help.warnings should contain(Messages.unexpectedOption("-z"))
        case other => fail(s"Expected Ior.Both, got $other")
      }
    }

    "handle a positional argument with unexpected trailing argument" in {
      positional.parseLenient(List("ok", "extra")) match {
        case Ior.Both(help, result) =>
          result should equal("ok")
          help.warnings should contain(Messages.unexpectedArgument("extra"))
        case other => fail(s"Expected Ior.Both, got $other")
      }
    }

    "handle combined options and positional with unexpected items" in {
      val opts = (whatever, positional).tupled
      opts.parseLenient(List("--whatever", "hello", "ok", "--extra", "trailing")) match {
        case Ior.Both(help, result) =>
          result should equal("hello" -> "ok")
          help.warnings should contain(Messages.unexpectedOption("--extra"))
          help.warnings should contain(Messages.unexpectedArgument("trailing"))
        case other => fail(s"Expected Ior.Both, got $other")
      }
    }

    "handle -- with unexpected arguments after it" in {
      val opts = (whatever, positional).tupled
      opts.parseLenient(List("--whatever", "hello", "--", "--ok", "extra")) match {
        case Ior.Both(help, result) =>
          result should equal("hello" -> "--ok")
          help.warnings should contain(Messages.unexpectedArgument("extra"))
        case other => fail(s"Expected Ior.Both, got $other")
      }
    }

    "handle interspersed arguments and options with unexpected items" in {
      val opts = (whatever, Opts.arguments[String]()).tupled
      opts.parseLenient(List("foo", "--whatever", "hello", "--extra", "bar")) match {
        case Ior.Both(help, result) =>
          result should equal("hello" -> NonEmptyList.of("foo", "bar"))
          help.warnings should contain(Messages.unexpectedOption("--extra"))
        case other => fail(s"Expected Ior.Both, got $other")
      }
    }

    "read a short option with unexpected trailing option" in {
      ghost.parseLenient(List("-g", "boo", "--unknown")) match {
        case Ior.Both(help, result) =>
          result should equal("boo")
          help.warnings should contain(Messages.unexpectedOption("--unknown"))
        case other => fail(s"Expected Ior.Both, got $other")
      }
    }

    "read a few short options with unexpected option" in {
      val force = Opts.flag("follow", short = "f", help = "...")
      val count = Opts.option[Int]("count", short = "n", help = "...")
      val file = Opts.arguments[String]("file")
      val opts = (force, count, file).tupled
      opts.parseLenient(List("first", "-fn30", "--extra", "second")) match {
        case Ior.Both(help, result) =>
          result should equal(((), 30, NonEmptyList.of("first", "second")))
          help.warnings should contain(Messages.unexpectedOption("--extra"))
        case other => fail(s"Expected Ior.Both, got $other")
      }
    }

    "handle alternative flags with unexpected option" in {
      val first = Opts.flag("first", help = "1").map(_ => 1)
      val second = Opts.flag("second", help = "2").map(_ => 2)
      val opts = first orElse second
      opts.parseLenient(List("--first", "--unknown")) match {
        case Ior.Both(help, result) =>
          result should equal(1)
          help.warnings should contain(Messages.unexpectedOption("--unknown"))
        case other => fail(s"Expected Ior.Both, got $other")
      }
    }

    "handle alternative arguments with unexpected argument" in {
      val one = Opts.argument[String]("single")
      val two = (Opts.argument[String]("left"), Opts.argument[String]("right")).tupled
      val opts = one orElse two
      opts.parseLenient(List("foo", "bar", "extra")) match {
        case Ior.Both(help, result) =>
          result should equal("foo" -> "bar")
          help.warnings should contain(Messages.unexpectedArgument("extra"))
        case other => fail(s"Expected Ior.Both, got $other")
      }
    }

    "handle subcommands with unexpected option in parent" in {
      val sub = Opts.subcommand("run", "...")(
        Opts.option[Int]("foo", help = "...").orNone
      )
      sub.parseLenient(List("--extra", "run", "--foo", "77")) match {
        case Ior.Both(help, result) =>
          result should equal(Some(77))
          help.warnings should contain(Messages.unexpectedOption("--extra"))
        case other => fail(s"Expected Ior.Both, got $other")
      }
    }

    "preserve warning order (chronological)" in {
      val opts = (whatever, ghost).tupled
      opts.parseLenient(List("--first", "--whatever", "x", "--second", "-g", "y", "third")) match {
        case Ior.Both(help, _) =>
          help.warnings should have size 3
          help.warnings(0) should equal(Messages.unexpectedOption("--first"))
          help.warnings(1) should equal(Messages.unexpectedOption("--second"))
          help.warnings(2) should equal(Messages.unexpectedArgument("third"))
        case other => fail(s"Expected Ior.Both, got $other")
      }
    }

    "accumulate multiple warnings of different types" in {
      val opts = (whatever, ghost).tupled
      opts.parseLenient(List("--unknown", "--whatever", "x", "-q", "-g", "y", "extra")) match {
        case Ior.Both(help, result) =>
          result should equal(("x", "y"))
          help.warnings should contain(Messages.unexpectedOption("--unknown"))
          help.warnings should contain(Messages.unexpectedOption("-q"))
          help.warnings should contain(Messages.unexpectedArgument("extra"))
        case other => fail(s"Expected Ior.Both, got $other")
      }
    }

    "fail with error for missing required options" in {
      val Ior.Left(help) = (whatever, ghost).tupled.parseLenient(List("--whatever", "x"))
      help.errors.exists(isMissing) shouldBe true
    }

    "fail and promote warnings to errors when validation fails" in {
      val validated = whatever.validate("must not be empty")(_.nonEmpty)
      validated.parseLenient(List("--whatever", "", "--extra")) match {
        case Ior.Left(help) =>
          help.errors should contain("must not be empty")
          help.errors should contain(Messages.unexpectedOption("--extra"))
        case other => fail(s"Expected Ior.Left, got $other")
      }
    }

    "fail on ambiguous option (flag vs option with same name)" in {
      val ambiguousFlag: Opts[Unit] = Opts.flag("xflag", help = "...", short = "x")
      val ambiguousOption: Opts[String] = Opts.option[String]("xoption", help = "...", short = "x")
      val ambiguousOpts = (ambiguousFlag, ambiguousOption).mapN((_, s) => s)
      val Ior.Left(help) = ambiguousOpts.parseLenient(List("-x", "value"))
      help.errors should contain(Messages.ambiguousOptionFlag("-x"))
    }

    "fail on missing value for option" in {
      val Ior.Left(help) = whatever.parseLenient(List("--whatever"))
      help.errors should contain(Messages.missingValueForOption("--whatever"))
    }

    "fail on unexpected value for flag" in {
      val Ior.Left(help) = flag.parseLenient(List("--test=value"))
      help.errors should contain(Messages.unexpectedValueForFlag("--test"))
    }

    "preserve warnings when hitting hard error (ambiguous option)" in {
      val ambiguousFlag: Opts[Unit] = Opts.flag("xflag", help = "...", short = "x")
      val ambiguousOption: Opts[String] = Opts.option[String]("xoption", help = "...", short = "x")
      val ambiguousOpts = (ambiguousFlag, ambiguousOption).mapN((_, s) => s)
      val Ior.Left(help) = ambiguousOpts.parseLenient(List("--unknown-first", "-x", "value"))
      help.errors should contain(Messages.ambiguousOptionFlag("-x"))
      help.errors should contain(Messages.unexpectedOption("--unknown-first"))
    }

    "preserve warnings when hitting hard error (missing value for option)" in {
      val Ior.Left(help) = whatever.parseLenient(List("--unknown-first", "--whatever"))
      help.errors should contain(Messages.missingValueForOption("--whatever"))
      help.errors should contain(Messages.unexpectedOption("--unknown-first"))
    }

    "preserve warnings when hitting hard error (unexpected value for flag)" in {
      val Ior.Left(help) = flag.parseLenient(List("--unknown-first", "--test=value"))
      help.errors should contain(Messages.unexpectedValueForFlag("--test"))
      help.errors should contain(Messages.unexpectedOption("--unknown-first"))
    }

    "propagate lenient mode into subcommands" in {
      val subOpt = Opts.option[Int]("sub-opt", help = "Subcommand option")
      val sub = Opts.subcommand("run", "Run subcommand")(subOpt)
      sub.parseLenient(List("run", "--sub-opt", "42", "--unexpected")) match {
        case Ior.Both(help, result) =>
          result should equal(42)
          help.warnings should contain(Messages.unexpectedOption("--unexpected"))
        case other => fail(s"Expected Ior.Both, got $other")
      }
    }

    "show subcommand Help when subcommand fails" in {
      val subOpt = Opts.option[Int]("sub-opt", help = "Subcommand option")
      val sub = Opts.subcommand("run", "Run subcommand")(subOpt)
      val mainOpt = Opts.option[String]("main-opt", help = "Main option")
      val mainWithSub = (mainOpt, sub).tupled
      val cmd = Command("main", "Main command")(mainWithSub)

      cmd.parseLenient(List("--main-opt", "x", "run")) match {
        case Ior.Left(help) =>
          help.errors.exists(isMissing) shouldBe true
          help.toString should include("--sub-opt")
          help.toString should include("Run subcommand")
        case other => fail(s"Expected Ior.Left, got $other")
      }
    }

    "show main Help when main command fails (even if subcommand also has error)" in {
      val subOpt: Opts[Int] = Opts.option[Int]("sub-opt", help = "Subcommand option")
      val sub: Opts[Int] = Opts.subcommand("run", "Run subcommand")(subOpt)
      val mainOpt: Opts[String] = Opts.option[String]("main-opt", help = "Main option")
      val mainWithSub: Opts[(String, Int)] = (mainOpt, sub).tupled
      val cmd = Command("main", "Main command")(mainWithSub)

      // Missing value for --main-opt (no value provided) AND missing --sub-opt in subcommand
      cmd.parseLenient(List("--main-opt", "run", "--sub-opt")) match {
        case Ior.Left(help) =>
          help.errors.exists(isMissing) shouldBe true
          // Should show main command's Help since main option is missing its value
          help.toString should include("--main-opt")
          help.toString should include("Main command")
        case other => fail(s"Expected Ior.Left, got $other")
      }
    }

    "show main Help when only main option fails" in {
      val subOpt: Opts[Int] = Opts.option[Int]("sub-opt", help = "Subcommand option")
      val sub: Opts[Int] = Opts.subcommand("run", "Run subcommand")(subOpt)
      val mainOpt: Opts[String] = Opts.option[String]("main-opt", help = "Main option")
      val mainWithSub: Opts[(String, Int)] = (mainOpt, sub).tupled
      val cmd = Command("main", "Main command")(mainWithSub)

      // Missing --main-opt but subcommand is valid
      cmd.parseLenient(List("run", "--sub-opt", "42")) match {
        case Ior.Left(help) =>
          help.errors.exists(isMissing) shouldBe true
          // Should show main command's Help since main option is missing
          help.toString should include("--main-opt")
          help.toString should include("Main command")
        case other => fail(s"Expected Ior.Left, got $other")
      }
    }

    "accumulate warnings from both parent and subcommand in lenient mode" in {
      val subOpt = Opts.option[Int]("sub-opt", help = "...")
      val sub = Opts.subcommand("run", "...")(subOpt)
      val mainOpt = Opts.option[String]("main-opt", help = "...")
      val mainWithSub = (mainOpt, sub).tupled

      mainWithSub.parseLenient(
        List("--main-opt", "x", "--parent-unknown", "run", "--sub-opt", "42", "--child-unknown")
      ) match {
        case Ior.Both(help, result) =>
          result should equal(("x", 42))
          help.warnings should contain(Messages.unexpectedOption("--parent-unknown"))
          help.warnings should contain(Messages.unexpectedOption("--child-unknown"))
        case other => fail(s"Expected Ior.Both, got $other")
      }
    }

    "fail fast in subcommand strict mode but not accumulate parent warnings" in {
      val subOptStrict: Opts[Int] = Opts.option[Int]("sub-opt", help = "...")
      val subStrict: Opts[Int] = Opts.subcommand("run", "...")(subOptStrict)
      val mainOptStrict: Opts[String] = Opts.option[String]("main-opt", help = "...")
      val mainWithSubStrict: Opts[(String, Int)] = (mainOptStrict, subStrict).tupled

      val Left(help) = mainWithSubStrict.parseStrict(
        List("--main-opt", "x", "run", "--sub-opt", "42", "--unexpected")
      )
      help.errors should have size 1
      help.errors should contain(Messages.unexpectedOption("--unexpected"))
    }
  }

  "Strict Parsing" should {

    val whatever = Opts.option[String]("whatever", help = "Useful!")
    val positional = Opts.argument[String]("expected")

    "fail immediately on unexpected option" in {
      val Left(help) = whatever.parseStrict(List("--whatever", "man", "--unknown"))
      help.errors should contain(Messages.unexpectedOption("--unknown"))
    }

    "fail immediately on unexpected argument" in {
      val Left(help) = positional.parseStrict(List("ok", "extra"))
      help.errors should contain(Messages.unexpectedArgument("extra"))
    }

    "not accumulate multiple unexpected items" in {
      val Left(help) = whatever.parseStrict(List("--whatever", "man", "--first", "--second"))
      help.errors should have size 1
      help.errors should contain(Messages.unexpectedOption("--first"))
    }

    "fail on unrecognized options, even with valid arguments" in {
      val Left(help) = whatever.parseStrict(List("--whatever=dude", "--unrecognized"))
      help.errors should contain(Messages.unexpectedOption("--unrecognized"))
    }
  }

}






