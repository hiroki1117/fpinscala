package hiroki1117.chapter2

import org.scalatest._
import Matchers._

class Chapter2Spec extends FlatSpec {

  "fib method" should "フィボナッチ数列" in {
    fib(10) should be (55)
    fib(15) should be (610)
   }
  
  "isSorted method" should "第二引数で確認される順序かどうかを判定する" in {
    isSorted[Int](Array(1,2,3,4,5), (a,b) => a <= b) should be (true)
    isSorted[Int](Array(1,2,3,4,5), (a,b) => a > b) should be (false)
  }

  "curry method" should "関数のカリー化をする" in {
    val adder = (a:Int, b:Int) => a + b
    val curried = curry(adder)
    adder.isInstanceOf[(Int,Int)=>Int] should be (true)
    curried.isInstanceOf[Int => Int] should be (true)
  }

  "uncurry method" should "カリー化された関数を解除する" in {
    val adder = (a:Int) => (b:Int) => a+b
    val uncurried = uncurry(adder)
    adder.isInstanceOf[Int=>Int=>Int] should be (true)
    uncurried.isInstanceOf[(Int,Int)=>Int] should be (true)
  }

  "compose method" should "関数を合成する" in {
    compose((x:Int) => x.toString ,(y:Int) => y + 10)(20) should be ("30")
  }
}
