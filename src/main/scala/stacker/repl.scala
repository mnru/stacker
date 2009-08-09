package stacker

import java.io.{BufferedReader, InputStreamReader}
import scala.io.Source
import scala.collection.mutable

object Stack {
  import scala.collection.mutable.Stack
  val stack = new Stack[Any]
  def length = stack.length

  def clear() = stack.clear

  def apply(): Any = {
    stack.pop
  }

  def apply(o: Any) {
    stack.push(o)
  }

  def peek: Any = {
    stack.top
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

object SymbolTable {
  val symbolTable = new mutable.HashMap[String, String]()

  def clear() = symbolTable.clear()

  def apply(word: String): Option[String] = {
    symbolTable.get(word)
  }

  def update(name: String, word: String) = {
    symbolTable += (name -> word)
    name
  }
}

class StackerEval extends Evaluator {
  def prompt = "%s] ".format(Stack.length)

  def defineWord(word: String): String = {
    val expr = word.substring(1, word.length).trim
    val idx = expr.indexOf(" ")
    val name = expr.substring(0, idx)
    val fn = expr.substring(idx, expr.length)
    SymbolTable(name) = fn
    name
  }

  def eval(body: String): String = {
    if (body.trim.startsWith(":")) {
      defineWord(body.trim) + "\nok"
    } else {
      body.trim.split(" ").map { cmd =>
        cmd match {
          case " " =>
          case "dup" => Stack.dup
          case "." => Stack.peek
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
          case item: String => try {
            println("TRACE: " + cmd)
            val number = item.toInt
            Stack(number)
          } catch {
            case _: NumberFormatException => {
              SymbolTable(item) match {
                case Some(word) => eval(word)
                case None => throw new IllegalArgumentException("unknown symbol: " + item); "bad"
              }
            }
          }
        }
      }.filter(!_.isInstanceOf[Unit]).mkString(" ") + "\nok"
    }
  }
}
