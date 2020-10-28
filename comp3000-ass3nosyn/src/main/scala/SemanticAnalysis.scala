/**
 * FunLang semantic analyser.
 *
 * Copyright 2020, Anthony Sloane, Kym Haines, Macquarie University, All rights reserved.
 */

package funlang

import FunLangTree.ExpTree
import org.bitbucket.inkytonik.kiama.attribution.Attribution

class SemanticAnalysis (tree : ExpTree) extends Attribution {

    import FunLangTree._
    import SymbolTable._
    import org.bitbucket.inkytonik.kiama.==>
    import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.{collect, collectall}
    import org.bitbucket.inkytonik.kiama.util.{Entity, MultipleEntity, UnknownEntity}
    import org.bitbucket.inkytonik.kiama.util.Messaging.{check, message}

    /**
     * Useful method to pretty-print types for error messages.
     */
    def prettyType (tipe : Type) : String =
        tipe match {
            case BoolType ()        => "Bool"
            case IntType ()         => "Int"
            case FunType (from, to) => s"${prettyType (from)} => ${prettyType (to)}"
            case UnknownType ()     => "Unknown"
        }

    /**
     * Collect the semantic error messages for a given tree.
     */
    val errors =
        attr (collectall {
            case d @ IdnDef (i) if entity (d) == MultipleEntity () =>
                message (d, i + " is declared more than once")

            case u @ IdnUse (i) if entity (u) == UnknownEntity () =>
                message (u, i + " is not declared")

            case e : Exp =>
                message (e, s"expected ${prettyType (exptipe (e))} got ${prettyType (tipe (e))}",
                         !iscompatible (tipe (e), exptipe (e))) ++
                (e match {
                    case AppExp (fn, arg) =>
                        tipe (fn) match {
                            case FunType (_, _) =>
                                Nil // OK, it's a function
                            case UnknownType () =>
                                Nil // OK, another error will flag this
                            case t =>
                                message (fn, s"application of non-function ${prettyType (t)}")
                        }
                    case _ =>
                        Nil // OK, not an AppExp so nothing extra to check
                })
        })

    /**
     * Are two types compatible?  If either of them are unknown then we
     * assume an error has already been raised elsewhere so we say they
     * are compatible with anything.
     */
    def iscompatible (t1 : Type, t2 : Type) : Boolean =
        t1 match {
            case FunType (from1, to1) =>
                t2 match {
                    case FunType (from2, to2) =>
                        iscompatible (from1, from2) && iscompatible (to1, to2)
                    case _ =>
                        t2 == UnknownType ()
                }
            case _ =>
                (t1 == UnknownType ()) || (t2 == UnknownType ()) || (t1 == t2)
        }

    /**
     * The entity defined by a defining occurrence of an identifier.
     * Defined by the context of the occurrence.
     */
    lazy val defentity : IdnDef => Entity =
        attr {
            case tree.parent(p) =>
                p match {
                    case defn : Arg => Argument (defn)
                    case defn : Fun => Function (defn)
                    case defn : Val => Value (defn)
                    case _          => UnknownEntity ()
                }
        }

    /**
     * The environment containing all bindings visible at a particular
     * node in the tree, not including any that are defined at that node.
     */
    val envin : FunLangNode => Environment =
        attr {

            // If we are at the program node (root of tree) then the
            // environment in is an empty root environment.
            case p : Program =>
                rootenv ()

            // If we are not the first child, just take the environment from
            // the previous sibling.
            case tree.prev(ps) =>
                env (ps)

            // If we are the first child of our parent, we get out environment
            // from the parent, pushing a new scope if the parent is a block
            // or a function arg/body (lambda).
            case tree.parent(np) =>
                np match {
                    case p : BlockExp =>
                        enter (envin (p))
                    case p : Lam =>
                        enter (envin (p))
                    case p : FunLangNode =>
                        envin (p)
                }
        }

    /**
     * The environment containing all bindings visible "after" a
     * particular node in the tree.  I.e., it's the environment at the
     * node plus any new bindings introduced by the node.
     */
    val env : FunLangNode => Environment =
        attr {

            // Definitions just add their entity to the environment
            case d : Definition =>
                define (envin (d), d.idndef.idn, entity (d.idndef))

            // Otherwise, if there are children we take the environment
            // coming out of the last child if there is one, otherwise
            // just pass the incoming environment back out. Nodes that
            // introduce scopes do the same, plus pop the scope.
            case n =>
            {
                val lastenv = n match {
                                  case tree.lastChild(lc) =>
                                      env(lc)
                                  case _ =>
                                      envin(n)
                              }
                n match {
                    case _ : BlockExp =>
                        leave (lastenv)
                    case _ : Lam =>
                        leave (lastenv)
                    case _ =>
                        lastenv
                }
            }
        }

    /**
     * The program entity referred to by an identifier definition or use.  In
     * the case of a definition it's the thing being defined, so define it to
     * be a reference to a new entity that represents that thing.  If it's
     * already defined, return an entity that indicates a multiple definition.
     * In the case of a use, it's the thing defined elsewhere that is being
     * referred to here, so look it up in the environment, using an unknown
     * entity if the environment does not already contain a binding.
     */
    val entity : Idn => Entity =
        attr {
            case n @ IdnDef (i) =>
                if (isDefinedInScope (envin (n), i))
                    MultipleEntity ()
                else
                    defentity (n)
            case n @ IdnUse (i) =>
                lookup (env (n), i, UnknownEntity ())
        }

    /**
     * What is the type of an expression?
     */
    val tipe : Exp => Type =
        attr {

            case AppExp (fn, e) if ! (e eq fn) =>
                tipe (fn) match {
                     case FunType (_, to) =>
                         to
                     case _ =>
                         UnknownType ()
                }

            case BlockExp (_, e) =>
                tipe (e)

            case BoolExp (_) =>
                BoolType ()

            case EqualExp (_, _) =>
                BoolType ()

            case n : IdnUse =>
                entity (n) match {
                    case Argument (Arg (_, tipe)) =>
                        tipe
                    case Function (Fun (_, Lam (Arg (_, argtipe), body))) =>
                        FunType (argtipe, tipe (body))
                    case Value (Val (_, exp)) =>
                        tipe (exp)
                    case _ =>
                        UnknownType ()
                }

            case IfExp (_, t, _) =>
                tipe (t)

            case IntExp (_) =>
                IntType ()

            case LessExp (_, _) =>
                BoolType ()

            case MinusExp (_, _) =>
                IntType ()

            case PlusExp (_, _) =>
                IntType ()

            case SlashExp (_, _) =>
                IntType ()

            case StarExp (_, _) =>
                IntType ()

        }

    /**
     * What is the expected type of an expression?
     */
    val exptipe : Exp => Type =
        attr {

            case tree.parent.pair(e, ep) =>
                ep match {

                    case AppExp (fn, arg) if e eq arg =>
                        tipe (fn) match {
                             case FunType (from, _) =>
                                 from
                             case _ =>
                                 UnknownType ()
                        }

                    case EqualExp (_, _) =>
                        IntType ()

                    case IfExp (c, _, _) if e eq c =>
                        BoolType ()

                    case IfExp (_, l, r) if e eq r =>
                        tipe (l)

                    case LessExp (_, _) =>
                        IntType ()

                    case MinusExp (_, _) =>
                        IntType ()

                    case PlusExp (_, _) =>
                        IntType ()

                    case SlashExp (_, _) =>
                        IntType ()

                    case StarExp (_, _) =>
                        IntType ()

                    case _  =>
                        UnknownType ()

                }

        }

}
