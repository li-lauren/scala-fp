package com.recfun

object recfun {
  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
    println("Parentheses Balancing")
    println(balance("(just an) example".toList))
    println(balance("I told him (that it’s not (yet) done). (But he wasn’t listening)".toList))
    println(balance(":-)".toList))
    println(balance("(()(".toList))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0) 1
    else if (c == r) 1
    else pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def updateCount(char: Char, count: Int) = {
      if (char == '(') count + 1
      else if (char == ')') count - 1
      else count
    }

    def _balance(chars: List[Char], count: Int): Boolean = {
      if (chars.isEmpty) {
        if (count != 0) false
        else true
      } else {
        val newCount = updateCount(chars.head, count)
        if (newCount < 0) false
        else if (chars.isEmpty && newCount != 0) false
        else true && _balance(chars.tail, newCount)
      }
    }

    _balance(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = ???
}
