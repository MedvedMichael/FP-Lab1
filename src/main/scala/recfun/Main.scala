package recfun

import scala.annotation.tailrec
import scala.math

object Main {
  def main(args: Array[String]): Unit = {
    for (row <- 0 to 50) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = pascalRecurs(c, r, List(1))

  @tailrec
  def pascalRecurs(column: Int, row: Int, lastLine: List[Int]): Int = {
    if (row == 0)
      return lastLine(column)

    var newArr: List[Int] = List(1)
    for (i <- 0 to lastLine.length - 2)
      newArr = newArr :+ (lastLine(i) + lastLine(i + 1))

    pascalRecurs(column, row - 1, newArr :+ 1)
  }

  /**
   * Exercise 2
   */


  def balance(chars: List[Char]): Boolean = balanceRecurs(chars, List())

  def balanceRecurs(chars: List[Char], stack: List[Char]): Boolean = {
    val brackets = List('(', ')', '{', '}', '[', ']')

    if (chars.isEmpty) {
      return stack.isEmpty
    }


    val currChar +: last = chars
    val index = brackets.indexOf(currChar)
    if (index == -1) {
      return balanceRecurs(last, stack)
    }

    if (index % 2 == 0)
      return balanceRecurs(last, stack :+ currChar)

    if (stack.isEmpty) return false
    val lastBracket +: newStack = stack
    if (lastBracket == brackets(index - 1))
      return balanceRecurs(last, newStack)
    false
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    countChangeRecurs(money, coins.sortWith((x, y) => x < y), List(), 0)

  def countChangeRecurs(money: Int, coins: List[Int], currentWay: List[Int], output: Int): Int = {
    if (money < 0)
      return output

    if (money == 0)
      return output + 1

    var newOutput = output
    for (index <- coins.indices) {
      newOutput = countChangeRecurs(money - coins(index), coins.slice(index, coins.length), currentWay :+ coins(index), newOutput)
    }
    newOutput

  }

  def calculateFunction5(x: Float): Double = {
    if (x <= 10)
      throw new Error("Invalid input")

    sum(x, 1, 8)
  }

  def calculateFunction5(x: Float, k: Float): Double = {
    if (x >= 10)
      throw new Error("Invalid input")

    k * scala.math.pow(x, k)
  }

  def sum(x: Float, start: Int, end: Int): Float = sumRecurs(x, start, end, 0)

  def sumRecurs(x: Float, start: Int, end: Int, result: Float): Float =
    if (start == end)
      result + x * start
    else
      sumRecurs(x, start + 1, end, result + x * start)

}
