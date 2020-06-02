package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = if (c < 0 || c > r) 0 else if (r == 0) 1 else pascal(c-1, r-1) + pascal(c, r-1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def bal_inner(chars: List[Char], stack: List[Char]): Boolean = {
      if (chars.isEmpty && stack.isEmpty) true
      else if (chars.isEmpty && stack.nonEmpty) false
      else {
        val head = chars.head
        if (head == '{' || head == '(' || head == '[') {
          bal_inner(chars.tail, List(head).concat(stack))
        }
        else if (head == '}') {
          if (stack.nonEmpty && stack.head == '{')
            bal_inner(chars.tail, stack.tail)
          else false
        }
        else if (head == ')') {
          if (stack.nonEmpty && stack.head == '(')
            bal_inner(chars.tail, stack.tail)
          else false
        }
        else if (head == ']') {
          if (stack.nonEmpty && stack.head == '[')
            bal_inner(chars.tail, stack.tail)
          else false
        }
        else
          bal_inner(chars.tail, stack)
      }
    }
  bal_inner(chars, List())

  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def inner (accum: => Int, coins: List[Int]): Int = {
      if (coins.isEmpty == true) 0
      else if (accum > money) 0
      else if (accum == money) 1
      else
        inner(accum+coins.head, coins) + inner(accum, coins.tail)
    }
    inner(0, coins.sorted)
  }
}
