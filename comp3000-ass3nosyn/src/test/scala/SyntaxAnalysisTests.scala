/**
 * FunLang syntax analysis tests.
 *
 * Copyright 2020, Anthony Sloane, Kym Haines, Macquarie University, All rights reserved.
 */

package funlang

import org.bitbucket.inkytonik.kiama.util.ParseTests
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * Tests that check that the syntax analyser works correctly.  I.e., it accepts
 * correct input and produces the appropriate trees, and it rejects illegal input.
 */
@RunWith(classOf[JUnitRunner])
class SyntaxAnalysisTests extends ParseTests {

    import FunLangTree._

    val parsers = new SyntaxAnalysis (positions)
    import parsers._

    // Tests of parsing terminals

    test ("parsing an identifier of one letter produces the correct tree") {
        idnuse ("x") should parseTo[FunLangNode] (IdnUse("x"))
    }

    test ("parsing an identifier as an identifier produces the correct tree") {
        idnuse ("count") should parseTo[FunLangNode] (IdnUse("count"))
    }

    test ("parsing an identifier containing digits and underscores produces the correct tree") {
        idnuse ("x1_2_3") should parseTo[FunLangNode] (IdnUse("x1_2_3"))
    }

    test ("parsing an integer as an identifier gives an error") {
        idnuse ("42") should failParseAt[FunLangNode] (1, 1,
                                                      "identifier expected")
    }

    test ("parsing a non-identifier as an identifier gives an error (digit)") {
        idnuse ("4foo") should failParseAt[FunLangNode] (1, 1,
                                                      "identifier expected")
    }

    test ("parsing a non-identifier as an identifier gives an error (underscore)") {
        idnuse ("_f3") should failParseAt[FunLangNode] (1, 1,
                                                      "identifier expected")
    }

    test ("parsing a keyword as an identifier gives an error") {
        idnuse ("def") should failParseAt[FunLangNode] (1, 1,
                                                      "failure of not")
    }

    test ("parsing a keyword prefix as an identifier produces the correct tree") {
        idnuse ("defat") should parseTo[FunLangNode] (IdnUse("defat"))
    }

    test ("parsing an integer of one digit as an integer produces the correct tree") {
        factor ("8") should parseTo[FunLangNode] (IntExp(8))
    }

    test ("parsing an integer as an integer produces the correct tree") {
        integer ("99") should parseTo[String] ("99")
    }

    test ("parsing a non-integer as an integer gives an error") {
        integer ("total") should failParseAt[String] (1, 1,
            "string matching regex '[0-9]+' expected but 't' found")
    }

    // Tests of parsing basic expressions

    test ("parsing an equal expression produces the correct tree") {
        exp ("a == 1") should parseTo[FunLangNode] (EqualExp (IdnUse ("a"), IntExp (1)))
    }

    test ("parsing a less than expression produces the correct tree") {
        exp ("a < 1") should parseTo[FunLangNode] (LessExp (IdnUse ("a"), IntExp (1)))
    }

    test ("parsing an addition expression produces the correct tree") {
        exp ("a + 1") should parseTo[FunLangNode] (PlusExp (IdnUse ("a"), IntExp (1)))
    }

    test ("parsing a subtraction expression produces the correct tree") {
        exp ("a - 1") should parseTo[FunLangNode] (MinusExp (IdnUse ("a"), IntExp (1)))
    }

    test ("parsing a multiplication expression produces the correct tree") {
        exp ("a * 1") should parseTo[FunLangNode] (StarExp (IdnUse ("a"), IntExp (1)))
    }

    test ("parsing a division expression produces the correct tree") {
        exp ("a / 1") should parseTo[FunLangNode] (SlashExp (IdnUse ("a"), IntExp (1)))
    }

    test ("parsing an integer expression produces the correct tree") {
        exp ("823") should parseTo[FunLangNode] (IntExp (823))
    }

    test ("parsing a true expression produces the correct tree") {
        exp ("true") should parseTo[FunLangNode] (BoolExp (true))
    }

    test ("parsing a false expression produces the correct tree") {
        exp ("false") should parseTo[FunLangNode] (BoolExp (false))
    }

    test ("parsing an identifier expression produces the correct tree") {
        exp ("v123") should parseTo[FunLangNode] (IdnUse ("v123"))
    }

    test ("parsing a parenthesized expression produces the correct tree") {
        exp ("(a + 5)") should parseTo[FunLangNode] (PlusExp (IdnUse ("a"), IntExp (5)))
    }

    test ("parsing an application expression produces the correct tree") {
        exp ("a (b)") should parseTo[FunLangNode] (AppExp (IdnUse ("a"), IdnUse ("b")))
    }

    test ("parsing an if expression produces the correct tree") {
        exp ("if (true) then 3 else 4") should parseTo[FunLangNode] (IfExp (BoolExp (true), IntExp (3), IntExp (4)))
    }

    // FIXME: more tests here...

}
