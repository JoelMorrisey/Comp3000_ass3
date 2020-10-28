/*
 * Name tests for FunLang compiler.
 *
 * Copyright (C) 2020 Anthony M Sloane, Kym Haines, Macquarie University, All rights reserved.
 */

package funlang

/**
 * Tests that check that the name analyser detects the appropriate errors.
 */
class NameTests extends SemanticTests {

    // Basic binding tests

    test ("a top-level variable is unbound") {
        val messages =
            semanticTest ("""
                |x
                """.stripMargin)
        assert (messages.length === 1)
        assertMessage (messages, 0, 2, 1, "x is not declared")
    }

    test ("a bound value is not an error") {
        val messages =
            semanticTest ("""
                |{
                |  val x = 1
                |  x
                |}
                """.stripMargin)
        assert (messages.length === 0)
    }

    test ("an outer binding is visible in an inner block") {
        val messages =
            semanticTest ("""
                |{
                |  val x = 1
                |  val y = { val z = 1 x }
                |  y
                |}
                """.stripMargin)
        assert (messages.length === 0)
    }

    test ("a bound value is not visible in its own definition") {
        val messages =
            semanticTest ("""
                |{
                |  val y = y
                |  y
                |}
                """.stripMargin)
        assert (messages.length === 1)
        assertMessage (messages, 0, 3, 11, "y is not declared")
    }

    test ("a binding doesn't escape to an outer scope") {
        val messages =
            semanticTest ("""
                |{
                |  val y = { val x = 1 x }
                |  x
                |}
                """.stripMargin)
        assert (messages.length === 1)
        assertMessage (messages, 0, 4, 3, "x is not declared")
    }

    test ("an unbound value is an error") {
        val messages =
            semanticTest ("""
                |{
                |  val x = 1
                |  y
                |}
                """.stripMargin)
        assert (messages.length === 1)
        assertMessage (messages, 0, 4, 3, "y is not declared")
    }

    test ("a rebinding is an error (val-val)") {
        val messages =
            semanticTest ("""
                |{
                |  val x = 1
                |  val x = 2
                |  x
                |}
                """.stripMargin)
        assert (messages.length === 1)
        assertMessage (messages, 0, 4, 7, "x is declared more than once")
    }

    test ("a rebinding is an error (val-def)") {
        val messages =
            semanticTest ("""
                |{
                |  val x = 1
                |  def x (y : Int) = 4
                |  x
                |}
                """.stripMargin)
        assert (messages.length === 1)
        assertMessage (messages, 0, 4, 7, "x is declared more than once")
    }

    test ("a rebinding is an error (def-val)") {
        val messages =
            semanticTest ("""
                |{
                |  def x (y : Int) = 4
                |  val x = 1
                |  x
                |}
                """.stripMargin)
        assert (messages.length === 1)
        assertMessage (messages, 0, 4, 7, "x is declared more than once")
    }

    // Bindings in functions

    test ("using a function argument name is not an error") {
        val messages =
            semanticTest ("""
                |{
                |  def f (x : Int) = x
                |  1
                |}
                """.stripMargin)
        assert (messages.length === 0)
    }

    test ("using an unbound function argument name is an error") {
        val messages =
            semanticTest ("""
                |{
                |  def f (x : Int) = a
                |  1
                |}
                """.stripMargin)
        assert (messages.length === 1)
        assertMessage (messages, 0, 3, 21, "a is not declared")
    }

    // Nested binding tests

    test ("a nested binding in a block goes away outside the block") {
        val messages =
            semanticTest ("""
                |{
                |  val y = {
                |    val x = 1
                |    x
                |  }
                |  x
                |}
                """.stripMargin)
        assert (messages.length === 1)
        assertMessage (messages, 0, 7, 3, "x is not declared")
    }

    test ("a nested binding in a lambda goes away outside the lambda (argument)") {
        val messages =
            semanticTest ("""
                |{
                |  def f (x : Int) = x
                |  x
                |}
                """.stripMargin)
        assert (messages.length === 1)
        assertMessage (messages, 0, 4, 3, "x is not declared")
    }

    test ("nested binding of different name is not an error (val-val)") {
        val messages =
            semanticTest ("""
                |{
                |  val x = 1
                |  val o = {
                |    val y = 2
                |    y
                |  }
                |  x
                |}
                """.stripMargin)
        assert (messages.length === 0)
    }

    test ("nested binding of different name is not an error (def-def)") {
        val messages =
            semanticTest ("""
                |{
                |  def f (x : Int) = x
                |  def g (y : Int) = y
                |  f
                |}
                """.stripMargin)
        assert (messages.length === 0)
    }

    test ("nested binding of different name is not an error (val-def)") {
        val messages =
            semanticTest ("""
                |{
                |  val x = 1
                |  def f (y : Int) = y
                |  x
                |}
                """.stripMargin)
        assert (messages.length === 0)
    }

    test ("nested binding of different name is not an error (def-val)") {
        val messages =
            semanticTest ("""
                |{
                |  def f (y : Int) = y
                |  val x = 1
                |  x
                |}
                """.stripMargin)
        assert (messages.length === 0)
    }

    test ("nested binding of same name is not an error (val-val)") {
        val messages =
            semanticTest ("""
                |{
                |  val x = 1
                |  val o = {
                |    val x = 2
                |    x
                |  }
                |  x
                |}
                """.stripMargin)
        assert (messages.length === 0)
    }

    test ("nested binding of same name is not an error (def-def)") {
        val messages =
            semanticTest ("""
                |{
                |  def f (x : Int) = x
                |  def g (x : Int) = x
                |  f
                |}
                """.stripMargin)
        assert (messages.length === 0)
    }

    test ("nested binding of same name is not an error (val-def)") {
        val messages =
            semanticTest ("""
                |{
                |  val x = 1
                |  def f (x : Int) = x
                |  x
                |}
                """.stripMargin)
        assert (messages.length === 0)
    }

    test ("nested binding of same name is not an error (def-val)") {
        val messages =
            semanticTest ("""
                |{
                |  def f (x : Int) = x
                |  val x = 1
                |  x
                |}
                """.stripMargin)
        assert (messages.length === 0)
    }

    test ("nested binding of outer name is not an error (val-val)") {
        val messages =
            semanticTest ("""
                |{
                |  val x = 1
                |  val o = {
                |    val y = 2
                |    x
                |  }
                |  x
                |}
                """.stripMargin)
        assert (messages.length === 0)
    }

    test ("nested binding of outer name is not an error (def-def)") {
        val messages =
            semanticTest ("""
                |{
                |  def f (x : Int) = x
                |  def g (x : Int) = {
                |    def h (f : Int) = f
                |    x
                |  }
                |  f
                |}
                """.stripMargin)
        assert (messages.length === 0)
    }

    test ("nested binding of outer name is not an error (val-def)") {
        val messages =
            semanticTest ("""
                |{
                |  val a = 1
                |  def g (x : Int) = {
                |    def h (a : Int) = a
                |    x
                |  }
                |  a
                |}
                """.stripMargin)
        assert (messages.length === 0)
    }

    test ("nested binding of outer name is not an error (def-val)") {
        val messages =
            semanticTest ("""
                |{
                |  def f (x : Int) = x
                |  val x = { val a = 1 f }
                |  x
                |}
                """.stripMargin)
        assert (messages.length === 0)
    }

    test ("nested use of unbound name is an error (val-val)") {
        val messages =
            semanticTest ("""
                |{
                |  val x = 1
                |  val o = {
                |    val x = y
                |    x
                |  }
                |  x
                |}
                """.stripMargin)
        assert (messages.length === 1)
        assertMessage (messages, 0, 5, 13, "y is not declared")
    }

    test ("nested use of unbound name is an error (def-def)") {
        val messages =
            semanticTest ("""
                |{
                |  def g (x : Int) = {
                |    def h (y : Int) = o
                |    x
                |  }
                |  g
                |}
                """.stripMargin)
        assert (messages.length === 1)
        assertMessage (messages, 0, 4, 23, "o is not declared")
    }

    // Recursive bindings

    test ("backward reference is allowed in same group") {
        val messages =
            semanticTest ("""
                |{
                |  def g (x : Int) = x
                |  def h (y : Int) = g (y)
                |  g
                |}
                """.stripMargin)
        assert (messages.length === 0)
    }

    test ("forward reference is not allowed in same group") {
        val messages =
            semanticTest ("""
                |{
                |  def g (x : Int) = h (x)
                |  def h (y : Int) = y
                |  g
                |}
                """.stripMargin)
        assert (messages.length === 1)
        assertMessage (messages, 0, 3, 21, "h is not declared")
    }

    test ("backward reference is allowed in different group") {
        val messages =
            semanticTest ("""
                |{
                |  def g (x : Int) = x
                |  val a = 1
                |  def h (y : Int) = g (y)
                |  g
                |}
                """.stripMargin)
        assert (messages.length === 0)
    }

    test ("forward reference is not allowed in different group") {
        val messages =
            semanticTest ("""
                |{
                |  def g (x : Int) = h (x)
                |  val a = 1
                |  def h (y : Int) = y
                |  g
                |}
                """.stripMargin)
        assert (messages.length === 1)
        assertMessage (messages, 0, 3, 21, "h is not declared")
    }

}
