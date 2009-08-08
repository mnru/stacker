package stacker

import java.io.{BufferedReader, InputStreamReader}
import scala.io.Source

object Stack {
  import scala.collection.mutable.Stack
  val stack = new Stack[Any]
  def length = stack.length

  def apply(): Any = {
    stack.pop
  }

  def apply(o: Any) {
    stack.push(o)
    o
  }

  def dup: Any = {
    val rv = stack.top
    this(rv)
    rv
  }
}

trait Evaluator {
  def prompt: String
  def eval(body: String): String
}

object Repl {
  val evaluator = new StackerEval
  def main(args: Array[String]): Unit = {
    val stdin = new BufferedReader(new InputStreamReader(System.in))
    while(true) {
      print(evaluator.prompt)
      println(eval(read(stdin)))
    }
  }

  def read(in: BufferedReader): String = in.readLine

  def eval(body: String): String = evaluator.eval(body)
}

class StackerEval extends Evaluator {
  def prompt = "%s] ".format(Stack.length)
  def eval(body: String): String = {
    val commands = body.split(" ")
    commands.map { cmd =>
      cmd match {
        case "dup" => Stack.dup
        case "." => println(Stack())
        case "q" => print(System.exit(0))
        case "+" => {
          // take two from Stack, add them, push them onto the stack.
          if (Stack.length < 2) {
            throw new RuntimeException("operator requires 2 stack elements, only %s found".format(Stack.length))
          }
          val first = Stack().asInstanceOf[Int]
          val second = Stack().asInstanceOf[Int]
          Stack(first + second)
        }
        case "0" => Stack(0)
        case item: String => if (item.toInt != 0) {
          Stack(item.toInt)
        }
      }
    }
    "ok"
  }
}
