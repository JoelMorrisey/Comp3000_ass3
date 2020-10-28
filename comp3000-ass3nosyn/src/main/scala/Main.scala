/**
 * FunLang implementation main program.
 *
 * Copyright 2020, Anthony Sloane, Kym Haines, Macquarie University, All rights reserved.
 */

package funlang

import FunLangTree.Program
import FunLangTree.ExpTree

/**
 * Conduct syntax analysis on the FunLang program in the file given as the
 * first command-line argument. If the file is syntactically valid, go on
 * to perform semantic checks, and if they pass, translate the code into
 * SEC code and run it.
 */
object Main {

    import java.io.FileNotFoundException
    import org.bitbucket.inkytonik.kiama.output.PrettyPrinter.{any, layout}
    import org.bitbucket.inkytonik.kiama.parsing.Success
    import org.bitbucket.inkytonik.kiama.util.{FileSource, Positions, PositionStore}
    import org.bitbucket.inkytonik.kiama.util.Messaging
    import org.bitbucket.inkytonik.kiama.util.OutputEmitter
    import Translator.translate

    def main (args : Array[String]) {

        args.size match {

            // If there is exactly one command-line argument, we want to
            // compile and run that file.
            case 1 =>
                try {
                    // Create a reader for the argument file name
                    val source = new FileSource (args (0))

                    // Create a syntax analysis module
                    val posns = new Positions
                    val parsers = new SyntaxAnalysis (posns)

                    // Create a messaging module for semantic analysis
                    val messaging = new Messaging with PositionStore {
                                       override val positions = posns
                                    }

                    // Parse the file
                    parsers.parse (parsers.parser, source) match {
                        // If it worked, we get a source tree
                        case Success (sourcetree, _) =>

                            // Pretty print the source tree
                            // println (layout (any (sourcetree)))

                            // Compile and run the program source tree
                            compileAndRun (sourcetree, messaging)

                        // Parsing failed, so report it
                        case f =>
                            println (f)
                    }
                } catch {
                    case e : FileNotFoundException =>
                        println (e.getMessage)
                }

            // Complain otherwise
            case _ =>
                println ("usage: run [file.fun]")

        }

    }

    /**
     * Compile and run a program source tree.
     */
    def compileAndRun (program : Program, messaging : Messaging) {

        val tree = new ExpTree (program)
        val analysis = new SemanticAnalysis (tree)
        import analysis._

        // For Assignment Two, just pretty-print the tree
        // println (pretty_any (exp))

        // Perform semantic checks for errors, print
        // report of messages if there were any
        val messages = errors (program)
        if (messages.length > 0)
            messaging.report (messages)
        else {

            // For Assignment Three, if there are no errors, perform the
            // translation into SEC instructions and then execute those
            // instructions on an SEC machine.

            // Translation
            val instrs = translate (program)
            println (instrs)

            // A machine to perform the run
            val machine = new SECMachine (new OutputEmitter)

            // Execution
            machine.run (instrs) match {
                case machine.FatalError (message) =>
                    println ("execution error: " + message)
                case _ =>
                    // All ok, do nothing
            }
        }

    }

}
