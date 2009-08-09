package stacker

import org.specs.Specification

object EvalSpec extends Specification {
  val eval = new StackerEval

  "Eval" should {
    doBefore {
      Stack.clear()
    }

    "1 1 + makes 2" >> {
      eval.eval("1 1 +") mustEqual "2\nok"
      Stack() mustEqual 2
    }

    "+ requires something on the stack" >> {
      Stack.length mustEqual 0
      eval.eval("+") must throwA[Exception]
    }

    ". does not remove anything from the Stack" >> {
      eval.eval("1 .") mustEqual "1\nok"
      Stack.length mustEqual 1
    }
  }
}
