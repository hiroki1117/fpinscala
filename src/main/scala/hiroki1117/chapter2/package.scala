package hiroki1117

import scala.annotation.tailrec

package object chapter2{
  def fib(n: Int): Int = {
    @tailrec
    def loop(i: Int, now: Int, next: Int): Int = {
      if(i==0) now
      else loop(i-1, next, now+next)
    }
    loop(n, 0, 1)
  }

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    var result = true
    for(x <- 0 until as.size-1) {
      result = ordered(as(x), as(x+1))
    }
    result
  }

  def curry[A,B,C](f: (A,B) => C): A=>(B=>C) = a => b => f(a,b)

  def uncurry[A,B,C](f: A=>B=>C): (A,B) => C = (a,b) => f(a)(b)

  def compose[A,B,C](f: B=>C, g: A=>B): A=>C = a => f(g(a))
}
