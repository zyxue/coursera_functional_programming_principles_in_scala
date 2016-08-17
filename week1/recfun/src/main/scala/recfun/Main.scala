package recfun
import common._
import scala.util.control.Breaks._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    // if (c == r || c == 0 || r == 0) {
    //   return 1
    // } else {
    //   return pascal(c - 1, r -1) + pascal(c, r - 1)
    // }

    // or

    if (c == r || c == 0 || r == 0) 1
    else pascal(c - 1, r -1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def checkBalance(chars: List[Char], count: Int): Boolean = {
      //count should never be less than 0 (e.g. )()
      if (count < 0) false
      else {
        if (chars.isEmpty) {
          if (count != 0) false
          else true
        }
        else {
          val c = chars.head

          // if (c == '(') {
          //   checkBalance(chars.tail, count + 1)
          // }
          // else if (c == ')') {
          //   checkBalance(chars.tail, count - 1)
          // }
          // else {
          //   checkBalance(chars.tail, count)
          // }

          // v.s. in terms of readability

          if (c == '(') checkBalance(chars.tail, count + 1)
          else if (c == ')') checkBalance(chars.tail, count - 1)
          else checkBalance(chars.tail, count)

        }
      }
    }

    checkBalance(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0) {
        return 1
      }

      if (money < 0) {
        return 0
      }

      if (coins.isEmpty && money > 0) {
        return 0
      }

      return countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
}
