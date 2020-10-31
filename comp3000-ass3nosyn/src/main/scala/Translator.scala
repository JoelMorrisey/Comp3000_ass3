/**
 * FunLang to SEC translator.
 *
 * Copyright 2020, Anthony Sloane, Kym Haines, Macquarie University, All rights reserved.
 */

package funlang

/**
 * Translator from FunLang source programs to SEC target programs.
 */
object Translator {

    import SECTree._
    import FunLangTree._
    import scala.collection.mutable.ListBuffer
    import SymbolTable._

    /**
     * Return a frame that represents the SEC instructions for a FunLang program.
     */
    def translate (program : Program) : Frame = {

        // An instruction buffer for accumulating the program instructions
        val programInstrBuffer = new ListBuffer[Instr] ()

        /**
         * Translate the program by translating its expression.
         */
        val expInstrs = translateExpression (program.exp)
        programInstrBuffer.appendAll (expInstrs)
        programInstrBuffer.append (IPrint ())

        // Gather the program's instructions and return them
        programInstrBuffer.result ()

    }

    /**
     * Translate an expression and return a list of the instructions that
     * form the translation.
     */
    def translateExpression (exp : Exp) : Frame = {

        // An instruction buffer for accumulating the expression instructions
        val expInstrBuffer = new ListBuffer[Instr] ()

        /**
         * Generate an instruction by appending it to the instruction buffer.
         */
        def gen (instr : Instr) {
            expInstrBuffer.append (instr)
        }

        /**
         * Generate a sequence of instructions by appending them all to the
         * instruction buffer.
         */
        def genall (frame : Frame) {
            expInstrBuffer.appendAll (frame)
        }

        /**
         * Generate code to make a closure (argName => body).
         */
        def genMkClosure (argName : String, body : Exp) {
            val bodyInstrs = translateExpression (body)
            gen (IClosure (argName, bodyInstrs :+ IPopEnv ()))
        }

        exp match {

            case IntExp (value) =>
                gen (IInt (value))

            case BoolExp (value) =>
                gen (IBool (value))

            case IdnUse (value) =>
                gen (IVar (value))

            case PlusExp (l, r) =>
                genall (translateExpression (l))
                genall (translateExpression (r))
                gen (IAdd ())
            
            case MinusExp (l, r) =>
                genall (translateExpression (l))
                genall (translateExpression (r))
                gen (ISub ())
            
            case StarExp (l, r) =>
                genall (translateExpression (l))
                genall (translateExpression (r))
                gen (IMul ())

            case SlashExp (l, r) =>
                genall (translateExpression (l))
                genall (translateExpression (r))
                gen (IDiv ())

            case EqualExp (l, r) =>
                genall (translateExpression (l))
                genall (translateExpression (r))
                gen (IEqual ())

            case LessExp (l, r) =>
                genall (translateExpression (l))
                genall (translateExpression (r))
                gen (ILess ())

            case IfExp(c, t, e) =>
                genall (translateExpression (c))
                gen(IBranch(translateExpression (t), translateExpression (e)))

            case AppExp(f, arg) =>
                genall (translateExpression (f))
                genall (translateExpression (arg))
                gen(ICall())

            case BlockExp(head +: defns, body) =>
                //match all the different possible block heads
                head match {
                    case Val(IdnDef(idn), exp) =>
                    {
                        genMkClosure(idn, BlockExp(defns, body))
                        genall ( translateExpression(exp) )
                        gen (ICall())
                    }
                    case FunGroup(Fun(IdnDef(idn), Lam(Arg(IdnDef(x),_), exp)) +: fundefns) => {
                        genMkClosure(idn, BlockExp(FunGroup(fundefns) +: defns, body))
                        genMkClosure(x, exp)
                        gen (ICall())
                    }
                    case FunGroup(Vector()) => {
                        genall ( translateExpression(BlockExp(defns, body)) )
                    }
                }

            case BlockExp(Vector(), exp) => {
                genall ( translateExpression(exp) )
            }
            
            case _ =>
                // FIXME: Add cases for other kinds of expression...

        }

        // Gather the expression's instructions and return them
        expInstrBuffer.result ()

    }

}
