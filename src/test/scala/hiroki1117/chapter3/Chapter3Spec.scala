package hiroki1117.chapter3

import org.scalatest._
import Matchers._

class Chapter3Spec extends FlatSpec {

  import hiroki1117.chapter3.List._

  val list = List(1,2,3,4,5)
  val list2 = List(6,7,8,9)

  "tail method" should "先頭以外" in {
    tail(list) should be(List(2,3,4,5))
    list.tail should be(List(2,3,4,5))
  }

  "sedHead method" should "先頭に10をセット" in {
    setHead(list, 10).headOption.get should be(10)
    list.setHead(10).headOption.get should be(10)
  }

  "drop method" should "3要素分削除" in {
    drop(list, 3) should be(List(4,5))
    list.drop(3) should be(List(4,5))
  }

  "dropWhile method" should "3以下のうちは削除" in {
    dropWhile(list)(_ <= 3) should be(List(4,5))
    list.dropWhile(_ <= 3) should be(List(4,5))
  }
}
