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

  /**
   * Returns a copy of whatever element is on the stack at position pos.
   */
  def look(pos: Int): Any = stack(pos)

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

  def getTop2(): (Int, Int) = {
    checkLength(2)
    (Stack().asInstanceOf[Int], Stack().asInstanceOf[Int])
  }

  var trace = false

  // whether or not we are reading in an immediate word
  var readImmediateWord = false
  val immediateWord = new mutable.ListBuffer[String]()

  /**
   * To implement:
   * test true/false -> boolean
   * test lt -> v2 < v1 then push true else false
   * test eq
   * test gt
   * sel -> Pop 3, if v3 is 0, then push v1 else v2. error if v3 is not number
   */
  def eval(body: String): String = {
    if (body.trim.startsWith(":")) {
      if (trace) { println("TRACE: define " + body) }
      defineWord(body.trim) + "\nok"
    } else {
      body.trim.split(" ").map { cmd =>
        if (trace) { println("TRACE: " + cmd) }
        cmd match {
          case "[" => readImmediateWord = true
          case "]" => {
            readImmediateWord = false
            Stack(immediateWord.mkString(" "))
            immediateWord.clear()
          }
          case x if readImmediateWord => immediateWord += x
          case "exec" => {
            val word = Stack()
            word match {
              case w: String => eval(w)
              case _ => throw new RuntimeException("can't execute immediate word as it's not a String")
            }
          }
          case "trace" => trace = true
          case "untrace" => trace = false
          case " " =>
          case "dup" => Stack.dup
          case "." => Stack.peek
          case "nget" => {
            val position = Stack().asInstanceOf[Int]
            Stack(Stack.look(position))
          }
          case "swap" => {
            val first = Stack()
            val second = Stack()
            Stack(first)
            Stack(second)
          }
          case "eq" => {
            val nums = getTop2()
            if (nums._1 == nums._2) {
              Stack("0")
            } else {
              Stack("1")
            }
          }
          case "lt" => {
            val nums = getTop2()
            if (nums._2.toInt < nums._1.toInt) {
              Stack("1")
            } else {
              Stack("0")
            }
          }
          case "gt" => {
            val nums = getTop2()
            if (nums._2.toInt > nums._1.toInt) {
              Stack("1")
            } else {
              Stack("0")
            }
          }
          // sel -> Pop 3, if v3 is 0, then push v1 else v2. error if v3 is not number
          case "sel" => {
            val nums = getTop2()
            val pred = Stack()
            if (pred == "0") {
              Stack(nums._1)
            } else {
              Stack(nums._2)
            }
          }
          case "q" => print(System.exit(0))
          case "-" => {
            val nums = getTop2()
            Stack(nums._2 - nums._1)
          }
          case "+" => {
            val nums = getTop2()
            Stack(nums._2 + nums._1)
          }
          case "/" => {
            val nums = getTop2()
            Stack(nums._2 / nums._1)
          }
          case "%" => {
            val nums = getTop2()
            Stack(nums._2 % nums._1)
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
