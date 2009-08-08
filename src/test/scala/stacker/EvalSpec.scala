package stacker

import org.specs.Specification

object EvalSpec extends Specification {
  val eval = new StackerEval

  "Eval" should {
    "1 1 + makes 2" >> {
      eval.eval("1 1 +") mustEqual "ok"
      Stack() mustEqual 2
    }

    "+ requires something on the stack" >> {
      Stack.length mustEqual 0
      eval.eval("+") must throwA[Exception]
    }
  }
}
