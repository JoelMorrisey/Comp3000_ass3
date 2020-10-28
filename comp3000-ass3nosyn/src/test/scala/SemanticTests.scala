/*
 * Semantic test support for FunLang compiler.
 *
 * Copyright (C) 2020 Anthony M Sloane, Kym Haines, Macquarie University, All rights reserved.
 */

package funlang

import org.bitbucket.inkytonik.kiama.util.Tests
import org.bitbucket.inkytonik.kiama.util.StringSource
import org.bitbucket.inkytonik.kiama.util.{Positions,PositionStore}

/**
 * General support for
 */
class SemanticTests extends SyntaxAnalysis(new Positions) with Tests {

    import FunLangTree._
    import org.bitbucket.inkytonik.kiama.util.Messaging
    import org.bitbucket.inkytonik.kiama.util.Messaging.Messages
    import org.bitbucket.inkytonik.kiama.parsing.{Success,Failure,Error}

    /**
     * Parse some test input and, if the parse succeeds with no input left,
     * return the program tree. If the parse fails, fail the test.
     */
    def parseProgram (str : String) : Program = {
        val posns = positions

        // Create a messaging module for semantic analysis
        val messaging = new Messaging with PositionStore {
                           override val positions = posns
                        }

        parseAll (program, new StringSource(str)) match {
            case Success (r, in) =>
                if (!in.atEnd) fail ("input remaining at " + in.pos)
                r
            case f : Error =>
                fail ("parse error: " + f)
            case f : Failure =>
                fail ("parse failure: " + f)
        }
    }

    /**
     * Parse some test input and run the semantic analyser over the resulting
     * tree (if the parse succeeds).
     */
    def semanticTest (str : String) : Messages = {
        val ast = parseProgram (str)
        val tree = new ExpTree (ast)
        val analysis = new SemanticAnalysis (tree)
        import analysis._

        val messages = errors (ast)
        // println (messages)
        messages
    }

    /**
     * Assert that a message was produced at a given position.
     */
    def assertMessage (messages : Messages, index : Int, line : Int, column : Int, msg : String) {
        val m = messages (index)
        assertResult (msg, "wrong text in message " + index) (m.label)
        //assertResult (line, "wrong line number in message " + index) (m.line)
        //assertResult (column, "wrong column number in message " + index) (m.column)
    }

}

