package hiroki1117

package object chapter4 {
  sealed trait Option[+A] {
    def map[B](f: A=>B): Option[B] = this.flatMap(a => Some(f(a)))

    def flatMap[B](f: A=>Option[B]): Option[B] = this match {
      case Some(v) => f(v)
      case None => None
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case Some(v) => v
      case None => default
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = this.map(Some(_)).getOrElse(ob)

    def filter(f: A=>Boolean): Option[A] = this.flatMap(a => if(f(a)) Some(a) else None)

  }
  case class Some[+A](content: A) extends Option[A]
  case object None extends Option[Nothing]


  object Option {
    def apply[A](a: => A): Option[A] = try{
      val b = a
      Some(b)
    } catch {
      case Exception => None
    }

    def mean(xs: Seq[Double]): Option[Double] =
      if (xs.isEmpty) None else Some(xs.sum / xs.length)

    def variance(xs: Seq[Double]): Option[Double] = {
      val m: Option[Double] = mean(xs)
      m.flatMap(ave => mean(xs.map(e => Math.pow(e-ave, 2))))
    }

    def lift[A,B](f:A=>B): Option[A]=>Option[B] = _ map f

    def map2[A,B,C](a:Option[A], b:Option[B])(f: (A,B)=>C): Option[C] =
      for {
        a1 <- a
        b1 <- b
      } yield f(a1, b1)

    def map3[A,B,C,D](a:Option[A], b:Option[B], c:Option[C])(f:(A,B,C)=>D):Option[D] =
      for {
        a1 <- a
        b1 <- b
        c1 <- c
      } yield f(a1,b1,c1)

    def map4[A,B,C,D,E](a:Option[A], b:Option[B], c:Option[C], d:Option[D])(f:(A,B,C,D)=> E): Option[E] =
      for{
        a1 <- a
        b1 <- b
        c1 <- c
        d1 <- d
      } yield f(a1,b1,c1,d1)

    def sequence[A](optionlist: List[Option[A]]): Option[List[A]] =
      optionlist.foldRight(Some(Nil): Option[List[A]]){(e, acc) => map2(e, acc)(_ :: _)}

    def traverse[A,B](a: List[A])(f: A=>Option[B]): Option[List[B]] =
      a.foldRight(Some(Nil): Option[List[B]]){(e, acc) => map2(f(e), acc)(_ :: _)}
  }
}
