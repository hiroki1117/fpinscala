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
  }
}
