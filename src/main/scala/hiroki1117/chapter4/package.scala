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
    def pure[A](a: =>A): Option[A] = apply(a)

    def apply[A](a: => A): Option[A] = try{
      val b = a
      Some(b)
    } catch {
      case _: Exception => None
    }

    def mean(xs: Seq[Double]): Option[Double] =
      if (xs.isEmpty) None else Some(xs.sum / xs.length)

    def variance(xs: Seq[Double]): Option[Double] = {
      val m: Option[Double] = mean(xs)
      m.flatMap(ave => mean(xs.map(e => Math.pow(e-ave, 2))))
    }

    def lift[A,B](f:A=>B): Option[A]=>Option[B] = _ map f

    def ap[A,B](a: Option[A])(f: Option[A=>B]): Option[B] = map2(a, f)((aa, ff)=>ff(aa))

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

    def map3bymap2[A,B,C,D](a:Option[A],b:Option[B],c:Option[C])(f:(A,B,C)=>D): Option[D] =
      map2(map2(a,b)((_,_)), c){ case ((a1,b1), c1) => f(a1,b1,c1)}

    def map[A,B](a: Option[A])(f:A=>B):Option[B] = map2(a, pure(()))((a1, _) => f(a1))
  }

  sealed trait Either[+E, +A] {
    def map[B](f: A=>B): Either[E, B] = this.flatMap(x => Right(f(x)))

    def flatMap[EE >: E, B](f: A=>Either[EE,B]): Either[EE,B] = this match {
      case Left(l) => Left(l)
      case Right(r) => f(r)
    }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Left(_) => b
      case Right(_) => this
    }

    def ap[EE >: E, B](f: Either[EE, A=>B]): Either[EE, B] =
      for {
        a1 <- this
        f1 <- f
      } yield f1(a1)

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A,B)=>C): Either[EE, C] = b.ap(ap(Either.pure(f.curried)))

    def map2normal[EE >: E, B, C](b: Either[EE, B])(f: (A,B)=>C): Either[EE, C] =
      for {
        a1 <- this
        b1 <- b
      } yield f(a1,b1)

    def map3[EE>:E, B,C,D](b: Either[EE,B], c: Either[EE,C])(f: (A,B,C)=>D): Either[EE,D] =
      map2(b)((_,_)).map2(c){ case ((a1,b1),c1) => f(a1,b1,c1)}
  }
  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

  object Either {
    def pure[E, A](a: A): Either[E,A] = Right(a)

    def Try[A](a: => A): Either[Exception, A] =
      try Right(a)
      catch {case e: Exception => Left(e)}

    def mean(xs: Seq[Double]): Either[String,Double] =
      if(xs.isEmpty) Left("empty")
      else Right(xs.sum / xs.length)

    def safeDiv(x:Int, y:Int): Either[Exception, Int] = Try(x / y)

    def sequence[E, A](es: List[Either[E,A]]): Either[E, List[A]] =
      es.foldRight(Right(Nil): Either[E, List[A]]){(e, acc) => e.map2(acc)(_ :: _)}

    def traverse[E, A, B](as: List[A])(f: A=>Either[E,B]): Either[E, List[B]] =
      as.foldRight(Right(Nil): Either[E, List[B]])((e, acc) => f(e).map2(acc)(_ :: _))
  }
}
