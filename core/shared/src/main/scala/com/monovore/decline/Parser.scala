package com.monovore.decline

import cats.Functor
import cats.data.Validated.{Invalid, Valid}
import cats.data.{Ior, NonEmptyList, Validated}
import cats.syntax.all._
import com.monovore.decline.Opts.Name
import com.monovore.decline.Parser.Accumulator.OrElse
import com.monovore.decline.ParseIssue.{Error, Warning}

import scala.annotation.tailrec
import scala.util.{Left, Right}

/** Parse mode for command-line parsing */
sealed trait ParseMode
object ParseMode {
  /** Strict mode: unexpected options/args cause immediate failure */
  case object Strict extends ParseMode
  /** Lenient mode: unexpected options/args are accumulated as warnings */
  case object Lenient extends ParseMode
}

private[decline] case class Parser[+A](command: Command[A])
    extends ((List[String], Map[String, String], ParseMode) => Ior[Help, A]) {

  import Parser._

  def apply(args: List[String], env: Map[String, String]): Either[Help, A] =
    apply(args, env, ParseMode.Strict) match {
      case cats.data.Ior.Left(h) => scala.util.Left[Help, A](h)
      case cats.data.Ior.Right(a) => scala.util.Right[Help, A](a)
      case cats.data.Ior.Both(h, _) => scala.util.Left[Help, A](h) // In strict mode, warnings are promoted to errors
    }

  def apply(args: List[String], env: Map[String, String], mode: ParseMode): Ior[Help, A] =
    evalResult(consumeAll(args, Accumulator.fromOpts(command.options, env), Nil), mode)

  private[this] val help = Help.fromCommand(command)

  private[this] def evalResult[A](out: Result[A], mode: ParseMode): Ior[Help, A] = {
    // Partition issues into errors and warnings (reverse to get chronological order)
    val reversedIssues = out.issues.reverse
    val parseErrors = reversedIssues.collect { case Error(msg) => msg }
    val warnings = reversedIssues.collect { case Warning(msg) => msg }
    val hasParseErrors = parseErrors.nonEmpty

    out.get match {
      case Invalid(failed) =>
        // Structural failure - all messages become errors
        val allErrors = failed.messages.distinct.toList ++ parseErrors.distinct ++ warnings.distinct
        Ior.left(help.withErrors(allErrors))

      case Valid(_) if hasParseErrors =>
        // Had parse errors - cannot produce A, all messages become errors
        val allErrors = parseErrors.distinct ++ warnings.distinct
        Ior.left(help.withErrors(allErrors))

      case Valid(fn) =>
        fn() match {
          case Invalid(validationMessages) =>
            // Validation failed - all messages become errors
            val allErrors = validationMessages.distinct ++ warnings.distinct
            Ior.left(help.withErrors(allErrors))

          case Valid(result) =>
            val distinctWarnings = warnings.distinct
            if (distinctWarnings.isEmpty) {
              Ior.right(result)
            } else {
              mode match {
                case ParseMode.Strict =>
                  // In strict mode, warnings are promoted to errors
                  Ior.left(help.withErrors(distinctWarnings))
                case ParseMode.Lenient =>
                  // In lenient mode, return both warnings and result
                  Ior.both(help.withWarnings(distinctWarnings), result)
              }
            }
        }
    }
  }

  def toOption[B](args: ArgOut[B]): Option[Accumulator[B]] =
    args.collect { case Right(a) => a }.reduceOption(Accumulator.OrElse(_, _))

  @tailrec
  private[this] def consumeAll(
      args: List[String],
      accumulator: Accumulator[A],
      issues: List[ParseIssue]
  ): Result[A] =
    args match {
      case LongOptWithEquals(option, value) :: rest =>
        accumulator.parseOption(Opts.LongName(option)) match {
          case Some(MatchFlag(_)) =>
            consumeAll(rest, accumulator, Error(s"Got unexpected value for flag: --$option") :: issues)
          case Some(MatchAmbiguous) =>
            consumeAll(rest, accumulator, Error(s"Ambiguous option/flag: --$option") :: issues)
          case Some(MatchOption(next)) => consumeAll(rest, next(value), issues)
          case Some(MatchOptArg(next)) => consumeAll(rest, next(Some(value)), issues)
          case None =>
            consumeAll(rest, accumulator, Warning(s"Unexpected option: --$option") :: issues)
        }
      case LongOpt(option) :: rest =>
        accumulator.parseOption(Opts.LongName(option)) match {
          case Some(MatchFlag(next)) => consumeAll(rest, next, issues)
          case Some(MatchAmbiguous) =>
            consumeAll(rest, accumulator, Error(s"Ambiguous option/flag: --$option") :: issues)
          case Some(MatchOptArg(next)) => consumeAll(rest, next(None), issues)
          case Some(MatchOption(next)) =>
            rest match {
              case Nil =>
                accumulator.result.withIssues(Error(s"Missing value for option: --$option") :: issues)
              case value :: rest0 => consumeAll(rest0, next(value), issues)
            }
          case None =>
            consumeAll(rest, accumulator, Warning(s"Unexpected option: --$option") :: issues)
        }
      case "--" :: rest => consumeArgs(rest, accumulator, issues)
      case ShortOpt(NonEmptyString(flag, tail)) :: rest =>

        @tailrec
        def consumeShort(
            char: Char,
            tail: String,
            accumulator: Accumulator[A],
            issues: List[ParseIssue]
        ): Either[Result[A], (List[String], Accumulator[A], List[ParseIssue])] =
          accumulator.parseOption(Opts.ShortName(char)) match {
            case Some(MatchAmbiguous) =>
              tail match {
                case "" => Right((rest, accumulator, Error(s"Ambiguous option/flag: -$char") :: issues))
                case NonEmptyString(nextFlag, nextTail) =>
                  consumeShort(nextFlag, nextTail, accumulator, Error(s"Ambiguous option/flag: -$char") :: issues)
              }
            case Some(MatchFlag(next)) =>
              tail match {
                case "" => Right((rest, next, issues))
                case NonEmptyString(nextFlag, nextTail) => consumeShort(nextFlag, nextTail, next, issues)
              }

            case Some(MatchOptArg(next)) =>
              tail match {
                case "" => Right((rest, next(None), issues))
                case value => Right((rest, next(Some(value)), issues))
              }

            case Some(MatchOption(next)) =>
              tail match {
                case "" =>
                  rest match {
                    case Nil =>
                      Left(accumulator.result.withIssues(Error(s"Missing value for option: -$char") :: issues))
                    case value :: rest0 => Right((rest0, next(value), issues))
                  }
                case _ => Right((rest, next(tail), issues))
              }
            case None =>
              tail match {
                case "" => Right((rest, accumulator, Warning(s"Unexpected option: -$char") :: issues))
                case NonEmptyString(nextFlag, nextTail) =>
                  consumeShort(nextFlag, nextTail, accumulator, Warning(s"Unexpected option: -$char") :: issues)
              }
          }

        consumeShort(flag, tail, accumulator, issues) match {
          case Right((newRest, newAccumulator, newIssues)) =>
            consumeAll(newRest, newAccumulator, newIssues)
          case Left(result) => result
        }
      case arg :: rest =>
        accumulator
          .parseSub(arg)
          .map { parseSubcommand =>
            parseSubcommand(rest).leftMap { _.withPrefix(List(command.name)) } match {
              case Left(h) =>
                // Convert Help errors to ParseIssue.Error
                val helpErrors = h.errors.map(Error(_))
                Result.fail.withIssues(helpErrors.reverse ++ issues)
              case Right(r) => r.withIssues(issues)
            }
          } match {
          case Some(out) => out
          case None =>
            toOption(accumulator.parseArg(arg)) match {
              case Some(next) => consumeAll(rest, next, issues)
              case None =>
                consumeAll(rest, accumulator, Warning(s"Unexpected argument: $arg") :: issues)
            }
        }
      case Nil => accumulator.result.withIssues(issues)
    }

  @tailrec
  private[this] def consumeArgs(
      args: List[String],
      accumulator: Accumulator[A],
      issues: List[ParseIssue]
  ): Result[A] =
    args match {
      case Nil => accumulator.result.withIssues(issues)
      case arg :: rest =>
        toOption(accumulator.parseArg(arg)) match {
          case Some(next) => consumeArgs(rest, next, issues)
          case None =>
            consumeArgs(rest, accumulator, Warning(s"Unexpected argument: $arg") :: issues)
        }
    }
}

private[decline] object Parser {

  sealed trait Match[+A]
  case class MatchFlag[A](next: A) extends Match[A]
  case class MatchOption[A](next: String => A) extends Match[A]
  case class MatchOptArg[A](next: Option[String] => A) extends Match[A]
  case object MatchAmbiguous extends Match[Nothing]

  object Match {
    implicit val functor: Functor[Match] = new Functor[Match] {
      override def map[A, B](fa: Match[A])(f: A => B): Match[B] = fa match {
        case MatchFlag(next) => MatchFlag(f(next))
        case MatchOption(next) => MatchOption(next andThen f)
        case MatchOptArg(next) => MatchOptArg(next andThen f)
        case MatchAmbiguous => MatchAmbiguous
      }
    }
  }

  type ArgOut[+A] = NonEmptyList[Either[Accumulator[A], Accumulator[A]]]

  /** Merges consecutive Left or Right elements using OrElse.
    * Rewritten to be stack-safe using an iterative approach.
    */
  def squish[A](argOut: ArgOut[A]): ArgOut[A] = {
    @tailrec
    def merge(
        current: Either[Accumulator[A], Accumulator[A]],
        rest: List[Either[Accumulator[A], Accumulator[A]]],
        acc: List[Either[Accumulator[A], Accumulator[A]]]
    ): List[Either[Accumulator[A], Accumulator[A]]] =
      rest match {
        case Nil => (current :: acc).reverse
        case next :: tail =>
          (current, next) match {
            case (Left(x), Left(y)) => merge(Left(OrElse(x, y)), tail, acc)
            case (Right(x), Right(y)) => merge(Right(OrElse(x, y)), tail, acc)
            case _ => merge(next, tail, current :: acc)
          }
      }

    val merged = merge(argOut.head, argOut.tail, Nil)
    NonEmptyList.fromListUnsafe(merged)
  }

  type Err[+A] = Validated[List[String], A]

  sealed trait Accumulator[+A] {
    def parseOption(name: Opts.Name): Option[Match[Accumulator[A]]]
    def parseArg(arg: String): ArgOut[A] = NonEmptyList.of(Left(this))
    def parseSub(command: String): Option[List[String] => Either[Help, Result[A]]]
    def result: Result[A]

    def mapValidated[B](fn: A => Err[B]): Accumulator[B] =
      Accumulator.Validate(this, fn)

    final def map[B](fn: A => B): Accumulator[B] = mapValidated(fn andThen Validated.valid)
  }

  val LongOpt = "--(.+)".r
  val LongOptWithEquals = "--(.+?)=(.+)".r
  val ShortOpt = "-(.+)".r

  object NonEmptyString {
    def unapply(string: String): Option[(Char, String)] =
      if (string.isEmpty) None
      else Some(string.charAt(0) -> string.substring(1))
  }

  object Accumulator {

    case class Pure[A](value: Result[A]) extends Accumulator[A] {

      override def parseOption(name: Name) = None

      override def parseSub(command: String) = None

      override def result = value

      override def mapValidated[B](fn: A => Err[B]): Accumulator[B] = Pure(value.mapValidated(fn))
    }

    def ap[A, B](left: Accumulator[A => B], right: Accumulator[A]): Accumulator[B] =
      (left, right) match {
        case (l, r) => Ap(l, r)
      }

    case class Ap[X, A](left: Accumulator[X => A], right: Accumulator[X]) extends Accumulator[A] {

      override def parseOption(name: Opts.Name) = {
        (left.parseOption(name), right.parseOption(name)) match {
          case (Some(leftMatch), None) => Some(leftMatch.map { ap(_, right) })
          case (None, Some(rightMatch)) => Some(rightMatch.map { ap(left, _) })
          case (None, None) => None
          case _ => Some(MatchAmbiguous)
        }
      }

      override def parseArg(arg: String) = {

        lazy val parsedRight = squish(right.parseArg(arg))

        squish(left.parseArg(arg))
          .flatMap {
            // Left side can't accept the argument: try the right
            case Left(newLeft) =>
              parsedRight.map {
                case Left(newRight) => Left(ap(newLeft, newRight))
                case Right(newRight) => Right(ap(newLeft, newRight))
              }
            case Right(newLeft) => NonEmptyList.of(Right(ap(newLeft, right)))
          }
      }

      override def parseSub(command: String) = {
        val leftSub =
          left
            .parseSub(command)
            .map(parser =>
              parser andThen {
                _.map(leftResult => (leftResult, right.result).mapN(_ apply _))
              }
            )

        val rightSub =
          right
            .parseSub(command)
            .map(parser =>
              parser andThen {
                _.map(rightResult => (left.result, rightResult).mapN(_ apply _))
              }
            )

        leftSub <+> rightSub
      }

      override def result = left.result ap right.result
    }

    case class OrElse[A](left: Accumulator[A], right: Accumulator[A]) extends Accumulator[A] {

      override def parseOption(name: Name) =
        (left.parseOption(name), right.parseOption(name)) match {
          case (Some(MatchFlag(l)), Some(MatchFlag(r))) =>
            Some(MatchFlag(OrElse(l, r)))
          case (Some(MatchOption(l)), Some(MatchOption(r))) =>
            Some(MatchOption(value => OrElse(l(value), r(value))))
          case (Some(_), Some(_)) => Some(MatchAmbiguous)
          case (l @ Some(_), None) => l
          case (None, r @ Some(_)) => r
          case (None, None) => None
        }

      override def parseArg(arg: String) =
        left.parseArg(arg) concatNel right.parseArg(arg)

      override def parseSub(command: String) =
        (left.parseSub(command), right.parseSub(command)) match {
          case (None, None) => None
          case (l, None) => l
          case (None, r) => r
          case (Some(l), Some(r)) =>
            Some(args =>
              (l(args), r(args)) match {
                case (lh @ Left(_), _) => lh
                case (_, rh @ Left(_)) => rh
                case (Right(lv), Right(rv)) => Right(lv <+> rv)
              }
            )
        }

      override def result = left.result <+> right.result

      override def mapValidated[B](fn: A => Err[B]): Accumulator[B] =
        OrElse(left.mapValidated(fn), right.mapValidated(fn))
    }

    case class Regular(names: List[Opts.Name], visibility: Visibility, values: List[String] = Nil)
        extends Accumulator[NonEmptyList[String]] {

      override def parseOption(name: Opts.Name) =
        if (names contains name) Some(MatchOption(value => copy(values = value :: values)))
        else None

      override def parseSub(command: String) = None

      def result =
        NonEmptyList
          .fromList(values.reverse)
          .map(Result.success)
          .getOrElse(visibility match {
            case Visibility.Normal => Result.missingFlag(names.head)
            case _ => Result.fail
          })
    }

    case class OptionalOptArg(
        names: List[Opts.Name],
        visibility: Visibility,
        reversedValues: List[Option[String]] = Nil
    ) extends Accumulator[NonEmptyList[Option[String]]] {

      override def parseOption(name: Opts.Name) =
        if (names contains name)
          Some(MatchOptArg(arg => copy(reversedValues = arg :: reversedValues)))
        else None

      override def parseSub(command: String) = None

      override def result =
        NonEmptyList
          .fromList(reversedValues.reverse)
          .map(Result.success)
          .getOrElse(visibility match {
            case Visibility.Normal => Result.missingFlag(names.head)
            case _ => Result.fail
          })
    }

    case class Flag(names: List[Opts.Name], visibility: Visibility, values: Int = 0)
        extends Accumulator[NonEmptyList[Unit]] {

      override def parseOption(name: Opts.Name) =
        if (names contains name) Some(MatchFlag(copy(values = values + 1)))
        else None

      override def parseSub(command: String) = None

      def result =
        NonEmptyList
          .fromList(List.fill(values)(()))
          .map(Result.success)
          .getOrElse(visibility match {
            case Visibility.Normal => Result.missingFlag(names.head)
            case _ => Result.fail
          })
    }

    case object Argument extends Accumulator[String] {

      override def parseArg(arg: String) = NonEmptyList.of(Right(Pure(Result.success(arg))))

      override def parseOption(name: Name) = None

      override def parseSub(command: String) = None

      override def result = Result.missingArgument
    }

    case class Arguments(stack: List[String]) extends Accumulator[NonEmptyList[String]] {

      override def parseArg(arg: String) = {
        val noMore = Pure(Result(Valid(() => Valid(NonEmptyList(arg, stack).reverse))))
        val yesMore = Arguments(arg :: stack)
        NonEmptyList.of(Right(OrElse(noMore, yesMore)))
      }

      override def parseOption(name: Name) = None

      override def parseSub(command: String) = None

      override def result: Result[NonEmptyList[String]] =
        NonEmptyList
          .fromList(stack.reverse)
          .map(Result.success)
          .getOrElse(Result.missingArgument)
    }

    case class Subcommand[A](name: String, action: Parser[A], env: Map[String, String])
        extends Accumulator[A] {

      override def parseOption(name: Name) = None

      override def parseSub(command: String) = {
        val actionWithEnv = (opts: List[String]) => action(opts, env)
        if (command == name) Some(actionWithEnv andThen { _ map Result.success }) else None
      }

      override def result = Result.missingCommand(name)
    }

    case class Validate[A, B](a: Accumulator[A], f: A => Validated[List[String], B])
        extends Accumulator[B] {

      override def parseOption(name: Opts.Name) =
        a.parseOption(name).map { _.map { copy(_, f) } }

      override def parseArg(arg: String) =
        a.parseArg(arg).map {
          case Left(newA) => Left(newA.mapValidated(f))
          case Right(newA) => Right(newA.mapValidated(f))
        }

      override def parseSub(command: String) =
        a.parseSub(command).map { _ andThen { _.map { _.mapValidated(f) } } }

      override def result = a.result.mapValidated(f)

      override def mapValidated[C](fn: B => Err[C]) = Validate(a, f andThen { _ andThen fn })
    }

    def repeated[A](opt: Opt[A]): Accumulator[NonEmptyList[A]] = opt match {
      case Opt.Regular(name, _, _, visibility) => Regular(name, visibility)
      case Opt.Flag(name, _, visibility) => Flag(name, visibility)
      case Opt.Argument(_) => Arguments(Nil)
      case Opt.OptionalOptArg(name, _, _, visibility) => OptionalOptArg(name, visibility)
    }

    def fromOpts[A](opts: Opts[A], env: Map[String, String]): Accumulator[A] = opts match {
      case Opts.Pure(a) => Accumulator.Pure(Result.success(a))
      case Opts.Missing => Accumulator.Pure(Result.fail)
      case Opts.HelpFlag(a) =>
        fromOpts(a, env).mapValidated(_ => Validated.invalid(Nil))

      case Opts.App(f, a) => Accumulator.ap(fromOpts(f, env), fromOpts(a, env))
      case Opts.OrElse(a, b) => OrElse(fromOpts(a, env), fromOpts(b, env))
      case Opts.Validate(a, validation) =>
        fromOpts(a, env).mapValidated(validation andThen { _.leftMap(_.toList) })
      case Opts.Subcommand(command) => Subcommand(command.name, Parser(command), env)
      case Opts.Single(opt) =>
        opt match {
          case Opt.OptionalOptArg(name, _, _, visibility) =>
            OptionalOptArg(name, visibility).map(_.toList.last)
          case Opt.Regular(name, _, _, visibility) =>
            Regular(name, visibility).map(_.toList.last)
          case Opt.Flag(name, _, visibility) =>
            Flag(name, visibility).map(_.toList.last)
          case Opt.Argument(_) => Argument
        }
      case Opts.Repeated(opt) => repeated(opt)
      case Opts.Env(name, _, _) =>
        Accumulator.Pure(
          env
            .get(name)
            .map(Result.success)
            .getOrElse(Result.missingEnvVar(name))
        )
    }
  }
}
