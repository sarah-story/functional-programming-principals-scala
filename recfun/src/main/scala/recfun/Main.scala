package recfun

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
      def loop(iter:Int, prev:Int):Int = {
        if(iter == c) getVal(prev, iter)
        else loop(iter + 1, getVal(prev, iter))
      }

      def getVal(prev:Int, iter:Int):Int = {
        if(iter == 0) 1
        else if(iter == 1) r
        else prev * (r - c)/(c + 1)
      }

      loop(0, 0)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      def loop(remaining: List[Char], checked: List[Char]):Boolean = remaining.headOption match {
        case None if checked.isEmpty ⇒ true
        case None ⇒ false
        case Some(')') ⇒ checked.headOption match {
          case Some('(') ⇒ loop(remaining.tail, checked.tail)
          case _ ⇒ false
        }
        case Some('(') ⇒ loop(remaining.tail, '(' :: checked)

      }

      loop(chars.filter(x ⇒ x == '(' || x == ')'), Nil)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if(money == 0) 1
      else if(coins.isEmpty || money < 0) 0
      else countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
  }
