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

  "take method" should "n個取る" in {
    stream.take(2).toList should be(List(1,2))
    Stream.take(stream)(2).toList should be(List(1,2))
  }

  "drop method" should "n個落とす" in {
    stream.drop(1).toList should be(List(2,3))
  }

  "exists method" should "存在したらtrue" in {
    stream.exists(_ == 2) should be (true)
    stream.existsByFR(_ == 100) should be(false)
  }

  "forAll method" should "全てに" in {
    stream.forAll(_ < 15) should be(true)
  }

  "takeWhile method" should "条件が満たされるうちは取得" in {
    stream.takeWhile(_<3).toList should be(List(1,2))
    stream.takeWhileByFR(_<3).toList should be(List(1,2))
    Stream.takeWhile(stream)(_<3).toList should be(List(1,2))
  }

  "find method" should "条件を満たした要素の一番最初を取得" in {
    stream.find(_ == 3) should be(Option(3))
    stream.find(_ > 10) should be(None)
  }

  "fibs method" should "フィボナッチ数列" in {
    Stream.fibs.take(10).toList should be(List(0,1,1,2,3,5,8,13,21,34))

    Stream.fibs zipAll(Stream.fibsByUnfold) take (10) forAll{case (x,y) => x==y} should be(true)
  }

  "startsWith method" should "引数Streamではじまっているか" in {
    stream.startsWith(Stream(1)) should be(true)
    stream.startsWith(Stream(1,2)) should be(true)
    stream.startsWith(Stream(1,2,3)) should be(true)
    stream.startsWith(Stream(1,2,3,4)) should be(false)
  }

  "tails method" should "末尾のリスト一覧" in {
    stream.tails.map(_.toList).toList should be(List(List(1,2,3),List(2,3),List(3),Nil))
  }

  "hasSubsequence method" should "文字列の一部を含んでいる" in {
    stream.hasSubsequence(Stream(2)) should be(true)
    stream.hasSubsequence(Stream(2,3)) should be(true)
    stream.hasSubsequence(Stream(1,3)) should be(false)
  }

  "scanRight method" should "畳み込みの計算過程を保持する" in {
    stream.scanRight(0)(_ + _).toList should be(List(1+2+3+0,2+3+0,3+0))
    stream.scanRight(Nil: List[Int])(_::_).toList should be(List(List(1,2,3), List(2,3), List(3)))
  }
}