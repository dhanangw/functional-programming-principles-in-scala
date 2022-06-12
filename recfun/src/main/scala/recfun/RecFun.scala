package recfun

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = 
    if r == 0 || r == 1 || c == 0 || c == r  then
      return 1

    val prevCol = c-1
    return pascal(prevCol, r - 1) + pascal(prevCol + 1, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = 

    def balanced(chars: List[Char], unbalancedParantheses: Int): Boolean =
      if chars.isEmpty then
        unbalancedParantheses == 0
      else if chars.head == '(' then
        balanced(chars.tail, unbalancedParantheses + 1)
      else if chars.head == ')' then
        unbalancedParantheses > 0 && balanced(chars.tail, unbalancedParantheses - 1)
      else
        balanced(chars.tail, unbalancedParantheses)

    return balanced(chars, 0)

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if money == 0 then
      1
    else if money < 0 || coins.isEmpty then
      0
    else
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
