package hiroki1117

package object chapter5 {
  sealed trait Stream[+A] {
    def headOption: Option[A] = this match {
      case Cons(h, _) => Some(h())
      case Empty => None
    }

    def toList: List[A] = this match {
      case Cons(h, t) => h() :: t().toList
      case Empty => Nil
    }

    def foldRight[B](z: =>B)(f: (A, =>B)=>B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case Empty => z
    }

    def map[B](f:A=>B): Stream[B] = this.foldRight(Stream.empty)(Stream.cons(f(_), _))

    def filter(p: A=>Boolean): Stream[A] = this.foldRight(Stream.empty)((e,acc)=> if(p(e)) Stream.cons(e, acc) else acc)

    def append[B>:A](other: =>Stream[B]): Stream[B] = this.foldRight(other)(Stream.cons(_, _))

    def flatMap[B](f: A=>Stream[B]): Stream[B] = this.foldRight(Stream.empty[B])((e,acc) => f(e) append acc)

    def take(n: Int): Stream[A] = if(n <= 0) Stream.empty else this match {
      case Cons(h, t) => Stream.cons(h(), t().take(n - 1))
      case Empty => Stream.empty
    }

    def drop(n: Int): Stream[A] = if(n <= 0) this else this match {
      case Cons(_, t) => t().drop(n-1)
      case Empty => Stream.empty
    }

    def takeWhile(p: A=>Boolean): Stream[A] = this match {
      case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
      case _ => Empty
    }

    def takeWhileByFR(p: A=>Boolean): Stream[A] = this.foldRight(Stream.empty[A]){(e, acc) => if(p(e)) Stream.cons(e, acc) else Stream.empty}

    def headOptionByFR: Option[A] = this.foldRight(None: Option[A])((e, _) => Some(e))

    def exists(p: A=>Boolean): Boolean = this match {
      case Cons(h,t) => p(h()) || t().exists(p)
      case Empty => false
    }

    def existsByFR(p: A=>Boolean): Boolean = this.foldRight(false)(p(_) || _)

    def forAll(p: A=>Boolean): Boolean = this.foldRight(true)(p(_) && _)

    def find(p: A=>Boolean): Option[A] = filter(p).headOption
  }
  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: ()=>A, t: ()=>Stream[A]) extends Stream[A]

  object Stream {

    def cons[A](hd: =>A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if(as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

    def constant[A](a: A): Stream[A] = cons(a, constant(a))

    def from(n: Int): Stream[Int] = cons(n, from(n+1))

  }
}
