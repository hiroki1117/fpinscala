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
}
