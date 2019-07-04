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

  "init method" should "最後の要素を削除" in {
    init(list) should be(List(1,2,3,4))
    list.init should be(List(1,2,3,4))
    assertThrows[Exception] {
      init(Nil)
    }
    assertThrows[Exception] {
      Nil.init
    }
  }

  "length method" should "リストの長さを返す" in {
    length(list) should be(5)
    list.length should be(5)
    lengthByFoldLeft(Nil) should be(0)
    Nil.lengthByFoldLeft should be(0)
  }

  "sum method" should "リストの要素の合計" in {
    list.sum should be(15)
  }

  "product method" should "リストの要素の積" in {
    list.product should be(120)
  }

  "reverse method" should "リストを反転" in {
    reverse(list) should be(List(5,4,3,2,1))
    list.reverse should be(List(5,4,3,2,1))
  }

  "append method" should "リストの結合" in {
    append(list, list2) should be(List(1,2,3,4,5,6,7,8,9))
    list.append(list2) should be(List(1,2,3,4,5,6,7,8,9))
  }

  "flat method" should "リストをフラットにする" in {
    List(List(1,2,3),List(4,5)).flat should be(list)
  }

  "addOne method" should "リストの要素に1足す" in {
    addOne(list) should be(List(2,3,4,5,6))
    list.addOne should be(List(2,3,4,5,6))
  }

  "doubleList2String method" should "doubleをStringに変換" in {
    doubleToStringList(List(5.toDouble)) should be(List("5.0"))
    List(5.toDouble).doubleList2StringList should be(List("5.0"))
  }
}
