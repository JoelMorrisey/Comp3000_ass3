/*
 * Type tests for FunLang compiler.
 *
 * Copyright (C) 2020 Anthony M Sloane, Kym Haines, Macquarie University, All rights reserved.
 */

package funlang

/**
 * Tests that check that the type analyser detects the appropriate errors.
 */
class TypeTests extends SemanticTests {

    import FunLangTree._

    /**
     * Parse some test input and check that its type is as expected.
     */
    def typeTest (str : String, expectedType : Type) {
        val ast = parseProgram (str)
        //val tipe = SemanticAnalysis.tipe (ast.exp)
        val tree = new ExpTree (ast)
        val analysis = new SemanticAnalysis (tree)
        val tipe = analysis.tipe(ast.exp)
        assertResult (expectedType) (tipe)
    }

    // Basic expression type tests

    test ("true is of Boolean type") {
        typeTest ("true", BoolType ())
    }

    test ("false is of Boolean type") {
        typeTest ("false", BoolType ())
    }

    // FIXME: more tests here...

    // Tests of type compatibility

    // test ("the left argument of an addition can't be a Boolean") {
    //     val messages = semanticTest ("true + 1")
    //     assert (messages.length === 1)
    //     assertMessage (messages, 0, 1, 1, "expected Int got Bool")
    // }

    // FIXME: more tests here...

}
