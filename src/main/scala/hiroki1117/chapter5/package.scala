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

    def map[B](f:A=>B): Stream[B] = this.foldRight(Stream.empty[B])((e,acc)=>Stream.cons(f(e), acc))

    def filter(p: A=>Boolean): Stream[A] = this.foldRight(Stream.empty[A])((e,acc)=> if(p(e)) Stream.cons(e, acc) else acc)

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

    def zip[B](s2: Stream[B]): Stream[(A,B)] = Stream.unfold((this, s2))(_ match {
      case (Cons(ha,ta), Cons(hb,tb)) => Some(((ta(), tb()), (ha(), hb())))
      case _ => None
    })

    def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = Stream.unfold((this, s2))(_ match {
      case (Empty, Empty) => None
      case (Empty, Cons(b, t)) => Some(((Stream.empty[A], t()), (None, Some(b()))))
      case (Cons(a,t), Empty) => Some(((t(), Stream.empty[B]), (Some(a()), Option.empty[B])))
      case (Cons(a,ta), Cons(b, tb)) => Some(((ta(),tb()), (Some(a()), Some(b()))))
    })

    def startsWith[A](a: Stream[A]): Boolean = zipAll(a) takeWhile (_ match {
      case (_, Some(_)) => true
      case _ => false
    }) forAll{case (x,y) => x==y}

    def tails: Stream[Stream[A]] = Stream.unfold(this){x => x match {
      case Cons(_, t) => Some((t(), x))
      case Empty => None
    }} append Stream(Stream.empty)

    def hasSubsequence[A](s: Stream[A]): Boolean = tails exists (_ startsWith s)

    def scanRight[B](z: B)(f:(A,B)=>B): Stream[B] = foldRight((z, Stream.empty[B])){case (a, (acc_z, acc_s)) => {
      val temp = f(a,acc_z)
      (temp, Stream.cons(temp, acc_s))
    }} _2
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

    def fibs: Stream[Int] = {
      def go(ini1: Int, ini2: Int): Stream[Int] = Stream.cons(ini1, go(ini2, ini1+ini2))
      go(0,1)
    }

    def unfold[A,S](z:S)(f: S=>Option[(S,A)]): Stream[A] = f(z) match {
      case Some((next, a)) => Stream.cons(a, unfold(next)(f))
      case None => Stream.empty
    }

    def fibsByUnfold: Stream[Int] = unfold((0,1))(_ match {
      case (0, 1) => Some((1,1), 0)
      case (x, y) => Some((y, x+y), x)
    })

    def fromByUnfold(n: Int): Stream[Int] = unfold(n)(x => Some(x+1, x))

    def constantByUnfold[A](a: A): Stream[A] = unfold(a)(x=>Some(x,x))

    def ones: Stream[Int] = unfold(1)(x=>Some(x,x))

    def mapByUnfold[A,B](a: Stream[A])(f: A=>B): Stream[B] = unfold(a)(_ match {
      case Empty => None
      case Cons(a, t) => Some((t(), f(a())))
    })

    def take[A](as: Stream[A])(n: Int): Stream[A] = unfold((as, n)){case (xs, num) =>
      if(num <= 0) None
      else xs match {
        case Cons(a, t) => Some(((t(), num-1), a()))
        case Empty => None
      }
    }

    def takeWhile[A](as: Stream[A])(f: A=>Boolean): Stream[A] = unfold(as)(_ match {
      case Empty => None
      case Cons(h, t) if f(h()) => Some((t(), h()))
      case _ => None
    })

    def zipWith[A,B,C](as: Stream[A], bs: Stream[B])(f:(A,B)=>C): Stream[C] = unfold((as,bs))(_ match {
      case (Empty, _) => None
      case (_, Empty) => None
      case (Cons(ha, ta), Cons(hb, tb)) => Some(((ta(), tb()), f(ha(), hb())))
    })
  }
}
