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
      case Cons(_, tail) => if(n<=0) this else tail.drop(n-1)
    }

    def dropWhile(f: A=>Boolean):List[A] = this match {
      case Cons(head, tail) => if(f(head)) tail.dropWhile(f) else this
      case Nil => Nil
    }

    def init: List[A] = this match {
      case Cons(head, Nil) => Nil
      case Cons(head, tail) => Cons(head, tail.init)
      case Nil => throw new Exception
    }

    def foldRight[B](z: B)(f: (A, B) => B): B = this match {
      case Cons(h, t) => f(h, t.foldRight(z)(f))
      case Nil => z
    }

    def foldLeft[B](z:B)(f: (B, A)=> B): B = {
      @scala.annotation.tailrec
      def loop(list: List[A], acc: B): B = list match {
        case Cons(h,t) => loop(t, f(acc, h))
        case Nil => acc
      }
      loop(this, z)
    }

    def length: Int = foldRight(0)((_,acc) => 1 + acc)

    def sum[B >: A :Numeric]: B = foldLeft(implicitly[Numeric[B]].zero)(implicitly[Numeric[B]].plus(_, _))

    def product[B >: A : Numeric]: B = foldLeft(implicitly[Numeric[B]].zero)(implicitly[Numeric[B]].times(_,_))

    def lengthByFoldLeft = foldLeft(0)((acc, _) => 1 + acc)

    def reverse: List[A] = foldLeft(Nil: List[A])((acc, e) => Cons(e, acc))

    def append[B >: A](other: List[B]): List[B] = foldRight(other)((ele, acc)=> Cons(ele, acc))

    def flat[B](implicit ev: A <:< List[B]): List[B] = foldRight(Nil: List[B])((ele, acc) => ele.append(acc))
  }
  case object Nil extends List[Nothing]
  case class Cons[+A](head:A, tailList:List[A]) extends List[A]

  object List{
    def apply[A](as: A*): List[A] = {
      if(as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
    }

    def tail[A](list: List[A]): List[A] = list match {
      case Cons(_, tail) => tail
      case Nil => throw new Exception
    }

    def setHead[A, B >: A](list: List[A], ele: B): List[B] = list match {
      case Cons(_, tail) => Cons(ele, tail)
      case Nil => throw new Exception
    }

    def drop[A](list: List[A], n: Int): List[A] = list match {
      case Cons(_, t) => if (n <= 0) list else drop(t, n - 1)
      case Nil => Nil
    }

    def dropWhile[A](list: List[A])(f: A=>Boolean): List[A] = list match {
      case Nil => Nil
      case Cons(h, t) => if(f(h)) dropWhile(t)(f) else list
    }

    def init[A](list: List[A]): List[A] = list match {
      case Cons(h, Nil) => Nil
      case Cons(head, tail) => Cons(head, init(tail))
      case Nil => throw new Exception
    }

    def foldRight[A, B](list: List[A], z: B)(f:(A,B) => B): B = list match {
      case Cons(h, t) => f(h, foldRight(t, z)(f))
      case Nil => z
    }

    def foldLeft[A,B](list: List[A], z:B)(f:(B,A)=>B):B = {
      @scala.annotation.tailrec
      def loop(l:List[A], acc:B):B = l match {
        case Cons(h,t) => loop(t, f(acc, h))
        case Nil => acc
      }
      loop(list, z)
    }

    def length[A](list: List[A]): Int = foldRight(list, 0)((_,acc)=>1+acc)

    def lengthByFoldLeft[A](list: List[A]): Int = foldLeft(list, 0)((acc, _)=> acc + 1)

    def reverse[A](list: List[A]): List[A] = foldLeft(list, Nil: List[A])((acc, ele)=> Cons(ele, acc))

    def append[A](l1: List[A], l2:List[A]): List[A] = foldRight(l1, l2)((ele, acc)=> Cons(ele, acc))

    def flat[A](list: List[List[A]]): List[A] = foldRight(list, Nil:List[A])((ele, acc) => append(ele, acc))
  }
}
