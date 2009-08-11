package stacker

import org.specs.Specification

object EvalSpec extends Specification {
  val eval = new StackerEval

  "Eval" should {
    doBefore {
      Stack.clear()
      SymbolTable.clear()
    }

    "1 1 + makes 2" >> {
      eval.eval("1 1 +") mustEqual "\nok"
      Stack() mustEqual 2
    }

    "1 1 - makes 0" >> {
      eval.eval("1 1 -") mustEqual "\nok"
      Stack() mustEqual 0
    }

    "+ requires something on the stack" >> {
      Stack.length mustEqual 0
      eval.eval("+") must throwA[Exception]
    }

    "1 1 / makes 1" >> {
      eval.eval("1 1 /") mustEqual "\nok"
      Stack() mustEqual 1
    }

    "1 1 % makes 0" >> {
      eval.eval("1 1 %") mustEqual "\nok"
      Stack() mustEqual 0
    }

    ". does not remove anything from the Stack" >> {
      eval.eval("1 .") mustEqual "1\nok"
      Stack.length mustEqual 1
    }

    ": defines a word" >> {
      eval.eval(": add1 1 +") mustEqual "add1\nok"
      SymbolTable("add1") mustEqual Some(" 1 +")
    }

    ": colon words execute correctly" >> {
      eval.eval(":add1 1 +") mustEqual "add1\nok"
      SymbolTable("add1") mustEqual Some(" 1 +")
      eval.eval("1 add1") mustEqual "\nok\nok"
      Stack.length mustEqual 1
    }

    "nget copies the given number (as v_i) from the stack positionally and places it on top of the stack" >> {
      eval.eval("1 2 1 nget") mustEqual "\nok"
      Stack() mustEqual 2
    }

    "swap swaps the place of the top two items on the stack" >> {
      eval.eval("1 2 swap") mustEqual "\nok"
      Stack() mustEqual 1
      Stack() mustEqual 2
    }

    "gt" >> {
      eval.eval("1 2 gt") mustEqual "\nok"
      Stack() mustEqual "0"

      eval.eval("1 0 gt") mustEqual "\nok"
      Stack() mustEqual "1"
    }

    "lt" >> {
      eval.eval("1 2 lt") mustEqual "\nok"
      Stack() mustEqual "1"

      eval.eval("2 1 lt") mustEqual "\nok"
      Stack() mustEqual "0"
    }

    "eq 0" >> {
      eval.eval("1 1 eq") mustEqual "\nok"
      Stack() mustEqual "0"
    }

    "eq 1" >> {
      eval.eval("1 2 eq") mustEqual "\nok"
      Stack() mustEqual "1"
    }

    "sel 0" >> {
      eval.eval("0 2 1 sel") mustEqual "\nok"
      Stack() mustEqual 2
    }

    "sel 1" >> {
      eval.eval("1 1 2 sel") mustEqual "\nok"
      Stack() mustEqual 1
    }
  }

  "immediate words" should {
    "are stored on the Stack" >> {
      eval.eval("[ 1 + ]") mustEqual "\nok"
      Stack() mustEqual "1 +"
    }

    "exec runs an immediate word on the Stack" >> {
      eval.eval("1 [ 1 + ] exec") mustEqual "\nok\nok"
      Stack() mustEqual 2
    }
  }
}
