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
    /////Testing arithetic expressions
    test ("Single constant number evaluates to the correct result") {
        execTest ("""
            |4
            """.stripMargin,
            "4")
    }

    test ("Single constant number long evaluates to the correct result") {
        execTest ("""
            |243214
            """.stripMargin,
            "243214")
    }

    test ("a simple addition expression evaluates to the correct result") {
        execTest ("""
            |4 + 3
            """.stripMargin,
            "7")
    }

    test ("a simple subtraction expression evaluates to the correct result") {
        execTest ("""
            |4 - 3
            """.stripMargin,
            "1")
    }

    test ("a simple multiply expression evaluates to the correct result") {
        execTest ("""
            |4 * 3
            """.stripMargin,
            "12")
    }

    test ("a simple divide expression evaluates to the correct result") {
        execTest ("""
            |20 / 5
            """.stripMargin,
            "4")
    }

    test ("Chained addition expression evaluates to the correct result") {
        execTest ("""
            |4 + 3 + 10
            """.stripMargin,
            "17")
    }

    test ("an mixed chained expression evaluates to the correct result") {
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

    test ("order of operations on an expression evaluates to the correct result") {
        execTest ("""
            |4 * ((3 + 2) - 4) / 2
            """.stripMargin,
            "2")
    }

    /////Testing booleans
    test ("a simple true bool expression evaluates to the correct result") {
        execTest ("""
            |true
            """.stripMargin,
            "true")
    }

    test ("a simple false bool expression evaluates to the correct result") {
        execTest ("""
            |false
            """.stripMargin,
            "false")
    }

    test ("a simple true equals bool expression evaluates to the correct result") {
        execTest ("""
            |1 == 1
            """.stripMargin,
            "true")
    }

    test ("a simple false equals bool expression evaluates to the correct result") {
        execTest ("""
            |1 == 2
            """.stripMargin,
            "false")
    }

    test ("a simple true less than bool expression evaluates to the correct result") {
        execTest ("""
            |3 < 5
            """.stripMargin,
            "true")
    }

    test ("a simple false less than bool expression evaluates to the correct result") {
        execTest ("""
            |100 < 10
            """.stripMargin,
            "false")
    }

    test ("a complex equals bool expression evaluates to the correct result") {
        execTest ("""
            |1*2+1/2+3 == 2/2+1*9-4
            """.stripMargin,
            "false")
    }

    test ("a complex less than bool expression evaluates to the correct result") {
        execTest ("""
            |1*2+1/2+3-9 < 2/2+1
            """.stripMargin,
            "true")
    }

    /////Testing if statements
    test ("a true if expression evaluates to the correct result") {
        execTest ("""
            |if (true) then 2+3  else 2+2
            """.stripMargin,
            "5")
    }

    test ("a false if expression evaluates to the correct result") {
        execTest ("""
            |if (false) then 2+3 else 2+2
            """.stripMargin,
            "4")
    }

    test ("a conditionally true (equals expression) if expression evaluates to the correct result") {
        execTest ("""
            |if (1 == 1) then 2+3  else 2+2
            """.stripMargin,
            "5")
    }

    test ("a conditionally false (equals expression) if expression evaluates to the correct result") {
        execTest ("""
            |if (1 == 2) then 2+3 else 2+2
            """.stripMargin,
            "4")
    }

    test ("a conditionally true (less than expression) if expression evaluates to the correct result") {
        execTest ("""
            |if (1 < 2) then 2+3  else 2+2
            """.stripMargin,
            "5")
    }

    test ("a conditionally false (less than expression) if expression evaluates to the correct result") {
        execTest ("""
            |if (2 < 1) then 2+3 else 2+2
            """.stripMargin,
            "4")
    }

    test ("a complex false result (using equals) from if expression evaluates to the correct result") {
        execTest ("""
            |if (1*20+10-5 == 2*10+5+2) then 5*12+6 else 2+2*7-2+98/2
            """.stripMargin,
            "63")
    }

    test ("a complex true result (using equals) from if expression evaluates to the correct result") {
        execTest ("""
            |if (1*20+10-5 == 2*10+5) then 5*12+6-2+10 else 2+2*7
            """.stripMargin,
            "74")
    }

    /////Testing block statements
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

    test ("an block expression with a mixture of functions and vals evaluates correct result") {
        execTest ("""
            |{
                def inc (a : Int) = a + 1
                val k = 5
                def abc (a : Int) = a + 1
                def bbb (a : Int) = if (4<k) then 5+inc(k)+a else 5+a
                val t = 10
                val l = 111
                abc(4) + inc (4) + t + k + 5 + bbb(55) + l
            }
            """.stripMargin,
            "207")
    }

    test ("an block expression with a mixture of functions and vals and if statements evaluates correct result") {
        execTest ("""
            |{
                def inc (a : Int) = a + 1
                val k = if (true) then 5 else 1
                def abc (a : Int) = if (4<k) then 5+inc(k)+a else 3+k
                val t = if (k<10) then 10 else k
                abc(4) * inc (4) + t / k
            }*
            {
                val l = 2
                l
            }/
            5+
            {
                def inc (a : Int) = a * 2
                inc (5)
            }
            """.stripMargin,
            "40")
    }

    test ("conditional statement, nested blocks and accessing environments inside block expression with only vals evaluates to the correct result") {
        execTest ("""
            |{
                val x = 10+5
                val y = if ( x == (x*2)/2 ) then x + 10 * x else 0
                val z = if ( x == 0 ) then 0 else
                {
                    val p = if (10 < x) then x else y
                    val m = if (p == y) then 0 else 
                    {
                        def inc (a : Int) = a * 2 + 10 + 3 / 5 +
                        {
                            def inc (a : Int) = if (5 == 5) then 9 else 7
                            val k = 5
                            def abc (a : Int) = if (5 == 6) then 7 else 8
                            def c (a: Bool) = a
                            def p (a: Int) = a == 10
                            val t = 10
                            val m = if (c(1==1)) then (if(p(10)) then (if (p(11)) then 2 else 5) else 0) else 6
                            abc(4) + inc (4) + t + k + 5 + m
                        }
                        def dec (a : Int) = a * 2 - 10 - 3 / 5
                        inc(dec(100))
                    }
                    p + m
                }
                x + y + z
            }
            """.stripMargin,
            "627")
    }
}