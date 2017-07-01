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
      if(c == 0 || c == r) return 1
      if(c < 0 ||  c > r) return 0
      pascal(c-1,r-1) + pascal(c,r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
        val charClean = chars filter (x => x == '(' || x == ')') // Clean up the string
        balanceHelper(charClean,0)
    }

    def balanceHelper(chars: List[Char], opens:Int):Boolean = {
        if(chars.isEmpty) return opens == 0 // if string is empty, check if all parenthesis are closed
        else if(chars.head == '(') return balanceHelper(chars.tail,opens+1) // if is a open Parentheses count it and recurse
        else if(chars.head == ')' && opens>0) return balanceHelper(chars.tail,opens-1)// if is a open Parentheses and there are something to close
        return false
    }
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
          if(money == 0) return 0 // no money no way
          val coinSorted = coins.filter(_ <= money).sorted.reverse // get only coin less than the money that i have in decreasing order
          if(coinSorted.isEmpty) return 0 // if there are no penny,bye
          // The number of change is given by the number of time i chane the current value minus the biggest coin plus the sum of time
          // using all the other time, plus one in case the coin has the same value of the money
          val nWithHeightCoin =  countChange( money - coinSorted.head,coinSorted)
          val nWithoutHeightCoin = countChange( money,coinSorted.tail)
          val HasSameValueOfCoin = if(money - coinSorted.head == 0 )1 else 0
          nWithHeightCoin + nWithoutHeightCoin + HasSameValueOfCoin
        }
  }
