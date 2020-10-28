/**
 * FunLang syntax analyser.
 *
 * Copyright 2020, Anthony Sloane, Kym Haines, Macquarie University, All rights reserved.
 */

package funlang

import org.bitbucket.inkytonik.kiama.parsing.Parsers
import org.bitbucket.inkytonik.kiama.util.Positions

/**
 * Module containing parsers for FunLang.
 */
class SyntaxAnalysis (positions : Positions) extends Parsers (positions) {

    import FunLangTree._
    import scala.language.postfixOps

    lazy val parser : PackratParser[Program] =
        phrase (program)

    lazy val program : PackratParser[Program] =
        exp ^^ Program

    lazy val exp : PackratParser[Exp] =
        ("if" ~> "(" ~> exp <~ ")") ~ ("then" ~> exp) ~ ("else" ~> exp) ^^ {
            case c ~ t ~ e => IfExp (c, t, e)
        } |
        exp1

    lazy val exp1 : PackratParser[Exp] =
        exp2 ~ ("==" ~> exp2) ^^ { case e ~ t => EqualExp (e, t) } |
        exp2 ~ ("<" ~> exp2) ^^ { case e ~ t => LessExp (e, t) } |
        exp2

    lazy val exp2 : PackratParser[Exp] =
        exp2 ~ ("+" ~> exp3) ^^ { case e ~ t => PlusExp (e, t) } |
        exp2 ~ ("-" ~> exp3) ^^ { case e ~ t => MinusExp (e, t) } |
        exp3

    lazy val exp3 : PackratParser[Exp] =
        exp3 ~ ("*" ~> factor) ^^ { case e ~ t => StarExp (e, t) } |
        exp3 ~ ("/" ~> factor) ^^ { case e ~ t => SlashExp (e, t) } |
        factor

    lazy val factor : PackratParser[Exp] =
        app |
        block |
        "false" ^^ (_ => BoolExp (false)) |
        "true" ^^ (_ => BoolExp (true)) |
        idnuse |
        integer ^^ (s => IntExp (s.toInt)) |
        "(" ~> exp <~ ")" |
        failure ("exp expected")

    lazy val app : PackratParser[AppExp] =
        idnuse ~ ("(" ~> exp <~ ")") ^^ {
            case f ~ arg => AppExp (f, arg)
        }

    lazy val block : PackratParser[BlockExp] =
        ("{" ~> definitions) ~ (exp <~ "}") ^^ {
            case ds ~ e => BlockExp (ds, e)
        }

    lazy val definitions : PackratParser[Vector[DefnGroup]] =
        defngroup+

    lazy val defngroup : PackratParser[DefnGroup] =
        (fundefn+) ^^ FunGroup |
        valdefn

    lazy val valdefn : PackratParser[Val] =
        ("val" ~> idndef) ~ ("=" ~> exp) ^^ {
            case i ~ e => Val (i, e)
        }

    lazy val fundefn : PackratParser[Fun] =
        ("def" ~> idndef) ~ ("(" ~> arg <~ ")") ~ ("=" ~> exp) ^^ {
            case i ~ arg ~ body => Fun (i, Lam (arg, body))
        }

    lazy val arg : PackratParser[Arg] =
        idndef ~ (":" ~> tipe) ^^ {
            case i ~ t => Arg (i, t)
        }

    lazy val tipe : PackratParser[Type] =
        basictipe ~ ("=>" ~> tipe) ^^ {
            case l ~ r => FunType (l, r)
        } |
        basictipe

    lazy val basictipe : PackratParser[Type] =
        "Bool" ^^ (_ => BoolType ()) |
        "Int" ^^ (_ => IntType ()) |
        "(" ~> tipe <~ ")"

    // NOTE: You should not need to change anything below here...

    lazy val integer =
        regex ("[0-9]+".r)

    lazy val idndef =
        identifier ^^ IdnDef

    lazy val idnuse =
        identifier ^^ IdnUse

    val keywordStrings =
        List ("def", "else", "false", "if", "then", "true", "val")

    lazy val keyword =
        keywords ("[^a-zA-Z0-9_]".r, keywordStrings)

    lazy val identifier =
        not (keyword) ~> identifierBase

    lazy val identifierBase =
        regex ("[a-zA-Z][a-zA-Z0-9_]*".r) |
        failure ("identifier expected")

    lazy val whitespaceParser : PackratParser[Any] =
        rep ( """\s""".r | comment)

    lazy val comment : PackratParser[Any] =
        "/*" ~ rep (not ("*/") ~ (comment | any)) ~ "*/" |
        "//.*(\n|\\z)".r

}
