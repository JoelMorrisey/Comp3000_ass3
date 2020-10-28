/**
 * Symbol table for the FunLang language.
 *
 * Copyright 2020, Anthony Sloane, Kym Haines, Macquarie University, All rights reserved.
 */

package funlang

import org.bitbucket.inkytonik.kiama.util.Environments

/**
 * Symbol table module containing facilities for creating and
 * manipulating FunLang language symbol information.
 */
object SymbolTable extends Environments {

    import FunLangTree.{Arg, Fun, Val}
    import org.bitbucket.inkytonik.kiama.util.Entity

    /**
     * An entity that denotes a plain value.
     */
    case class Value (defn : Val) extends Entity

    /**
     * An entity that denotes a function argument.
     */
    case class Function (defn : Fun) extends Entity

    /**
     * An entity that denotes a function argument.
     */
    case class Argument (defn : Arg) extends Entity

}
