package hiroki1117.chapter4


import org.scalatest._
import Matchers._

class Chapter4Spec extends FlatSpec {

  val option = Option("hello")
  val option2 = Option(7)
  val none: Option[String] = None
  val sequenceList = List(Option(1), Option(2), Option(3))
  val list = List("hello", "world", "!")

  "map method" should "要素に処理を提要" in {
    option.map(_.length) should be(Option(5))
    none.map(_.length) should be(None)
  }

  "flatMap method" should "モナディックな関数を適用" in {
    option.flatMap{x=>Option(x.length)} should be(Option(5))
    none.flatMap(x=>Option(x.length)) should be(None)
  }

  "getOrElse method" should "Noneの場合デフォルト値" in {
    none.getOrElse("Default") should be("Default")
  }

  "orElse method" should "Noneの場合Optionalなデフォルト値" in {
    none.orElse(Option("Default")) should be(Option("Default"))
  }

  "filter method" should "条件に合うものを残す" in {
    option2.filter(_ > 5) should be(Option(7))
    option2.filter(_ < 5) should be(None)
  }

  "map2 method" should "2つのOptin値を受け取り処理を適用" in {
    Option.map2(option,option)(_.length + _.length) should be(Option(10))
    Option.map2(none, option)(_.length + _.length) should be(None)
  }

  "sequence method" should "List[Option[A]]をOption[List[A]]に変換" in {
    Option.sequence(sequenceList) should be(Option(List(1,2,3)))

    val test = None :: sequenceList

    Option.sequence(test) should be(None)
  }

  "traverse method" should "モナディク関数適用後にsequence" in {
    Option.traverse(list)(x=>Option(x.length)) should be(Option(List(5,5,1)))

    Option.traverse(list){x=>if(x.length>1)Some(x.length)else None} should be(None)
  }
}
