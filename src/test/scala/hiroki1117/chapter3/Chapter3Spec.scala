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

  "foldL method" should "リスト演算を逆順にする" in {
    list.foldLeft(Nil: List[Int])((acc,e)=> Cons(e,acc)) should be(list.reverse)
    foldLeft(list,Nil: List[Int])((acc,e)=> Cons(e,acc)) should be(list.reverse)
  }

  "foldR method" should "リスト演算で同じ" in {
    list.foldRight(Nil: List[Int])((e, acc) => Cons(e, acc)) should be(list)
    foldRight(list, Nil: List[Int])((e, acc) => Cons(e, acc)) should be(list)
  }

  "filter method" should "3以上を削除" in {
    list.filter(_>=3) should be(List(1,2))
    filterByFlatMap(list)(_>=3) should be(List(1,2))
  }

  "removeOdd method" should "奇数を削除" in {
    list.removeOdd should be(List(2,4))
    removeOdd(list) should be(List(2,4))
  }

  "hasSequence method" should "部分を持っている" in {
    hasSubsequence(list, List(2,3)) should be(true)
    hasSubsequence(list, List(5)) should be(true)
    hasSubsequence(list, List(1,2,3,4,5)) should be(true)
    hasSubsequence(list, List(1,2,4)) should be(false)
  }

  import Tree._
  val tree: Tree[Int] = Branch(Branch(Leaf(1), Leaf(2)),Branch(Leaf(3),Leaf(4)))

  "size method" should "木のサイズを計算" in {
    size(tree) should be(4)
  }

  "maximum method" should "木の値の最大値" in {
    maximum(tree) should be(4)
  }

  "depth method" should "木の深さを測る" in {
    depth(tree) should be(2)
  }

  "map method" should "木の構造を変えず処理を適用" in {
    Tree.map(tree)(_+1) should be(Branch(Branch(Leaf(2), Leaf(3)),Branch(Leaf(4),Leaf(5))))
  }

  "fold method" should "全要素を足す" in {
    fold(tree)(identity)((x,y) => x+y) should be(10)
  }
}
