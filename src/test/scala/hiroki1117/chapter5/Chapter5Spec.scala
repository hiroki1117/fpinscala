package hiroki1117.chapter5

import org.scalatest._
import Matchers._

class Chapter5Spec extends FlatSpec {

  val stream = Stream(1,2,3)

  "headOption method" should "先頭の要素をOptionで取得" in {
    stream.headOption should be(Option(1))
    Stream.empty[Int].headOption should be(None)
    stream.headOptionByFR should be(Option(1))
    Stream.empty[Int].headOptionByFR should be(None)
  }

  "toList method" should "Listに変換" in {
    stream.toList should be(List(1,2,3))
    Stream.empty[String].toList should be(Nil)
  }

  "foldRight method" should "右から適用" in {
    stream.foldRight(Nil: List[Int])(_ :: _).toList should be (List(1,2,3))
  }

  "filter method" should "奇数を削除" in {
    stream.filter(_%2==1).toList should be(List(1,3))
  }

  "append method" should "末尾に追加" in {
    (stream append stream).toList should be(List(1,2,3,1,2,3))
  }

}
