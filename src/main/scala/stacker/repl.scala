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

  def checkLength(needed: Int) {
    val size = Stack.length
    if (size < needed) {
      throw new RuntimeException("operator requires %s stack elements, only %s found".format(needed, size))
    }
  }

  var trace = false


  /**
   * To implement:
   * [] -> immediate words
   * exec -> run an immediate word found on the stack
   * div -> division
   * rem -> remained
   * true/false -> boolean
   * lt -> v2 < v1 then pop true else false
   * eq
   * gt
   * swap -> position swap
   * sel -> Pop 3, if v3 is 0, then push v1 else v2. error if v3 is not number
   * nget -> copies the given number (as v_i) from the stack positionally and places it on top of the stack
   */
  def eval(body: String): String = {
    if (body.trim.startsWith(":")) {
      if (trace) { println("TRACE: define " + body) }
      defineWord(body.trim) + "\nok"
    } else {
      body.trim.split(" ").map { cmd =>
        if (trace) { println("TRACE: " + cmd) }
        cmd match {
          case "trace" => trace = true
          case "untrace" => trace = false
          case " " =>
          case "dup" => Stack.dup
          case "." => Stack.peek
          case "q" => print(System.exit(0))
          case "-" => {
            checkLength(2)
            val v1 = Stack().asInstanceOf[Int]
            val v2 = Stack().asInstanceOf[Int]
            Stack(v2 - v1)
          }
          case "+" => {
            checkLength(2)
            val v1 = Stack().asInstanceOf[Int]
            val v2 = Stack().asInstanceOf[Int]
            Stack(v2 + v1)
          }
          case "0" => Stack(0)
          case item: String => try {
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
