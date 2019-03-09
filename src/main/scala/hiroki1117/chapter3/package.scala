package hiroki1117

package object chapter3 {
  sealed trait List[+A]{
    def tail: List[A] = this match {
      case Cons(_, tail) => tail
      case _ => throw new Exception
    }

    def tailOption: Option[List[A]] = this match {
      case Cons(_, tail) => Some(tail)
      case _ => None
    }

    def setHead[B >: A](ele: B): List[B] = this match {
      case Cons(_, tail) => Cons(ele, tail)
      case Nil => throw new Exception
    }

    def drop(n: Int): List[A] = this match {
      case Nil => Nil
      case Cons(_, tail) => if(n<=0) tail else tail.drop(n-1)
    }

    def dropWhile(f: A=>Boolean):List[A] = this match {
      case Cons(head, tail) => if(f(head)) tail.dropWhile(f) else this
      case Nil => Nil
    }

    def init: List[A] = this match {
      case Cons(head, tail) => Cons(head, tail.init)
      case Cons(head, Nil) => List(head)
      case Nil => Nil
    }

  }
  case object Nil extends List[Nothing]
  case class Cons[+A](head:A, tail:List[A]) extends List[A]

  object List{
    def apply[A](as: A*): List[A] = {
      if(as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
    }
  }
}
