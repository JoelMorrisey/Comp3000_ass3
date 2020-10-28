/**
 * SEC machine interpreter.
 *
 * Copyright 2020, Anthony Sloane, Kym Haines, Macquarie University, All rights reserved.
 */

package funlang

import org.bitbucket.inkytonik.kiama.output.PrettyPrinter
import org.bitbucket.inkytonik.kiama.util.Emitter

/**
 * Interpreter for SEC machine instructions. The `emitter` is used to perform
 * output.
 */
class SECMachine (emitter : Emitter) extends PrettyPrinter {

    import SECTree._

    /**
     * The status of the current interpreter execution.
     */
    sealed abstract class Status

    /**
     * Runtime state: a stack of operands to be operated on by the instructions,
     * a stack of environments that bind free variables, and a stack of instruction
     * frames that are still to be executed,.
     */
    case class State (opnds : List[MValue], envs : List[Environ], code : List[Frame]) extends Status

    /**
     * Runtime error: a fatal error has occurred, described by message.
     */
    case class FatalError (message : String) extends Status

    /**
     * Interpret a sequence of instructions in an empty initial environment
     * and return the status at the end of the execution.
     */
    def run (frame : Frame) : Status = {

        def loop (state : State) : Status = {

            val State (opnds, envs, code) = state

            code match {

                // No more instructions, we're done
                case List () =>
                    state

                // No more instructions in current frame, execute other frames
                case List () :: codetail =>
                    loop (State (opnds, envs, codetail))

                // Execute first instruction in the current frame. If it doesn't
                // result in a fatal error, continue with the rest of the code.
                case (i :: is) :: codetail =>
                    exec (i, State (opnds, envs, is :: codetail)) match {
                        case state : State =>
                            loop (state)
                        case error =>
                            error
                    }

            }

        }

        // Start execution with an empty operand stack, an empty environment,
        // and a single code frame containing the program code.
        loop (State (List (), List (Map.empty), List (frame)))

    }

    /**
     * Interpret a single instruction.
     */
    def exec (instr : Instr, state : State) : Status = {

        val State (opnds, envs, code) = state

        // Tracing: uncomment these to print out each instruction or other parts
        // of the state before each instruction is executed.
        // println (s"instr = $instr")

        /**
         * Helper function to lookup a name in the current environment and return
         * the value to which it is bound. Return `Some (v)` if a binding to
         * `v` exists, otherwise return `None`. The relevant environment is the
         * top-most one on the environment stack (if there is one).
         */
        def lookup (envs : List[Environ], name : String) : Option[MValue] =
            envs match {
                case env :: _ =>
                    env.get (name)
                case _ =>
                    None
            }

        /**
         * Helper function for arithmetic instruction implementation. Pop two
         * integers from the operand stack, apply `op` to them and push the
         * result onto the operand stack. If the expected operands are not there,
         * raise a fatal error.
         */
        def arithmetic (opname : String, op : (Int, Int) => Int) =
            opnds match {
                case MInt (r) :: MInt (l) :: opndstail =>
                    State (MInt (op (l, r)) :: opndstail, envs, code)
                case _ =>
                    FatalError ("int and int expected in " + opname)
            }

        // Dispatch the instruction to its implementation

        instr match {

            case IAdd () =>
                arithmetic ("add", _ + _)

            case IBool (value) =>
                State (MBool (value) :: opnds, envs, code)

            case IBranch (left, right) =>
                opnds match {
                    case MBool (value) :: opndstail =>
                        State (opndstail, envs, (if (value) left else right) :: code)
                    case _ =>
                        FatalError ("bool expected for branch")
                }

            case ICall () =>
                opnds match {
                    case value :: MClosure (argName, frame, env) :: opndstail =>
                        State (opndstail, env + ((argName -> value)) :: envs, frame :: code)
                    case _ =>
                        FatalError ("value and closure expected for call")
                }

            case IClosure (argName, body) =>
                envs match {
                    case env :: _ =>
                        val closure = MClosure (argName, body, env)
                        State (closure :: opnds, envs, code)
                    case _ =>
                        FatalError ("no environment for a closure")
                }

            case IDiv () =>
                opnds match {
                    case MInt (r) :: MInt (l) :: opndstail =>
                        if (r == 0)
                            FatalError ("division by zero")
                        else
                            State (MInt (l / r) :: opndstail, envs, code)
                    case _ =>
                        FatalError ("int and int expected in div")
                }

            case IEqual () =>
                opnds match {
                    case MBool (r) :: MBool (l) :: opndstail =>
                        State (MBool (l == r) :: opndstail, envs, code)
                    case MInt (r) :: MInt (l) :: opndstail =>
                        State (MBool (l == r) :: opndstail, envs, code)
                    case _ =>
                        FatalError ("int and int, or bool and bool expected in equal")
                }

            case IInt (value)  =>
                State (MInt (value) :: opnds, envs, code)

            case ILess () =>
                opnds match {
                    case MInt (r) :: MInt (l) :: opndstail =>
                        State (MBool (l < r) :: opndstail, envs, code)
                    case _ =>
                        FatalError ("int and int expected in less")
                }

            case IMul () =>
                arithmetic ("mul", _ * _)

            case IPopEnv () =>
                envs match {
                    case _ :: envstail =>
                        State (opnds, envstail, code)
                    case _ =>
                        FatalError ("no environment to popenv")
                }

            case IPrint () =>
                opnds match {
                    case value :: opndstail =>
                        value match {
                            case MInt (value) =>
                                emitter.emitln (value)
                            case MBool (value) =>
                                emitter.emitln (value)
                            case MClosure (argName, _, _) =>
                                emitter.emitln ("function of " + argName)
                        }
                        State (opndstail, envs, code)
                    case _ =>
                        FatalError ("value expected for print")
                }

            case ISub () =>
                arithmetic ("sub", _ - _)

            case IVar (name) =>
                lookup (envs, name) match {
                    case Some (value) =>
                        State (value :: opnds, envs, code)
                    case None =>
                        FatalError ("unknown variable " + name)
                }

        }

    }

}
