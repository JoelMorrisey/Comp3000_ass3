/**
 * FunLang language execution tests.
 *
 * Copyright 2020, Anthony Sloane, Kym Haines, Macquarie University, All rights reserved.
 */

package funlang

import FunLangTree.ExpTree

/**
 * Tests that check that the translation works correctly.
 */
class ExecTests extends SemanticTests {

    import org.bitbucket.inkytonik.kiama.util.StringEmitter

    /**
     * Parse some test input, perform semantic analysis checks, expect no
     * semantic errors. Then translate into SEC machine code and run the code.
     * The value `expected` should be the output that we expect from this
     * run.
     */
    def execTest (str : String, expected : String) {
        val tree = parseProgram (str)
        val analysis = new SemanticAnalysis (new ExpTree (tree))
        import analysis._
        val messages = analysis.errors (tree)
        // println (messages)
        assert (messages.length === 0)

        val instrs = Translator.translate (tree)
        // println ("\n"+instrs+"\n")

        val emitter = new StringEmitter ()
        val machine = new SECMachine (emitter)

        machine.run (instrs) match {
            case _ : machine.State =>
                // Terminated correctly in some state
                assertResult (expected + "\n", "wrong execution output") (emitter.result ())
            case machine.FatalError (message) =>
                fail (message)
        }
    }

    test ("an integer expression evaluates to the correct result") {
        execTest ("""
            |1
            """.stripMargin,
            "1")
    }

    test ("an addition expression evaluates to the correct result") {
        execTest ("""
            |3 + 4
            """.stripMargin,
            "7")
    }

    // FIXME: more tests here...
    /**
    TODO:
        * test applied occurances once they are able to be defined (part 2)
        * test applied functions (Application expression) (part 5)
    */

    /////Testing basic arithetic
    test ("an subtraction expression evaluates to the correct result") {
        execTest ("""
            |4 - 3
            """.stripMargin,
            "1")
    }

    test ("an multiply expression evaluates to the correct result") {
        execTest ("""
            |4 * 3
            """.stripMargin,
            "12")
    }

    test ("an divide expression evaluates to the correct result") {
        execTest ("""
            |20 / 5
            """.stripMargin,
            "4")
    }

    test ("an mixed expression evaluates to the correct result") {
        execTest ("""
            |4 - 3 + 10
            """.stripMargin,
            "11")
    }

    test ("an complex expression evaluates to the correct result") {
        execTest ("""
            |4 * 3 + 10 - 4 / 2
            """.stripMargin,
            "20")
    }

    /////Testing booleans
    test ("an simple true bool expression evaluates to the correct result") {
        execTest ("""
            |true
            """.stripMargin,
            "true")
    }

    test ("an simple false bool expression evaluates to the correct result") {
        execTest ("""
            |false
            """.stripMargin,
            "false")
    }

    test ("an simple true equals bool expression evaluates to the correct result") {
        execTest ("""
            |1 == 1
            """.stripMargin,
            "true")
    }

    test ("an simple false equals bool expression evaluates to the correct result") {
        execTest ("""
            |1 == 2
            """.stripMargin,
            "false")
    }

    /////Testing if statements
    test ("an true if expression evaluates to the correct result") {
        execTest ("""
            |if (1 == 1) then 2+3  else 2+2
            """.stripMargin,
            "5")
    }

    test ("an false if expression evaluates to the correct result") {
        execTest ("""
            |if (1 == 2) then 2+3 else 2+2
            """.stripMargin,
            "4")
    }

    /////Testing block statements
    test ("example block expression given evaluates correctly") {
        execTest ("""
                |{
                    val x = 100
                    def inc (a : Int) = a + 1
                    inc (x)
                }
                """.stripMargin,
                "101")
    }

    test ("an block expression with only vals evaluates to the correct result") {
        execTest ("""
            |{
                val x = 10+5
                val y = 10
                x + y
            }
            """.stripMargin,
            "25")
    }

    test ("an block expression with a mixture of functions and vals evaluates correct result") {
        execTest ("""
            |{
                def inc (a : Int) = a + 1
                val k = 5
                def abc (a : Int) = a + 1
                val t = 10
                abc(4) + inc (4) + t + k + 5
            }*
            {
                val l = 2
                l
            }
            """.stripMargin,
            "60")
    }

    test ("an block expression with a mixture of functions and vals and if statements evaluates correct result") {
        execTest ("""
            |{
                def inc (a : Int) = a + 1
                val k = if (true) then 5 else 1
                def abc (a : Int) = if (4<k) then 5+inc(k)+a else 3+k
                val t = if (k<10) then 10 else k
                abc(4) * inc (4) + t / k
            }
            """.stripMargin,
            "77")
    }
}