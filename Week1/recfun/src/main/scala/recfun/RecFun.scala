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
  def pascal(c: Int, r: Int): Int = {
    if(c == 0 || c == r)
      return 1
    else if(c > r)
      return 0
    
    return pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceCalculation(chars: List[Char], leftCount: Int): Boolean = {
      if(chars.isEmpty)
        return leftCount == 0
      else if(chars.head == '(')
        return balanceCalculation(chars.tail, leftCount + 1)
      else if(chars.head == ')')
        return if leftCount > 0 then balanceCalculation(chars.tail, leftCount - 1) else false
      else
        balanceCalculation(chars.tail, leftCount)
    }

    return balanceCalculation(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(money == 0)
      return 1
    else if(coins.isEmpty)
      return 0

    var ansCount = 0
    var targetCoin = coins.head
    var currentValue = 0

    while(currentValue <= money) {
      ansCount = ansCount + countChange(money - currentValue, coins.tail)
      currentValue = currentValue + targetCoin
    }

    return ansCount
  }
