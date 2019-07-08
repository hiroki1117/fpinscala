package hiroki1117

package object chapter10 {

  trait Monoid[A] {
    def op(a1: A, a2: A): A
    def zero: A
  }

  object Monoid {

    def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)

    val stringMonoid = new Monoid[String] {
      def op(a1: String, a2:String): String = a1+a2

      def zero = ""
    }

    def foldMap[A,B](as: List[A], m: Monoid[B])(f: A=>B): B = as.foldLeft(m.zero)((acc,e) => m.op(acc, f(e)))

    def foldRight[A,B](as: List[A], z:B)(f: (A,B) => B): B = foldMap(as.map(f.curried(_)).reverse, endoMonoid[B])(identity)(z)

    def foldLeft[A,B](as: List[A], z: B)(f: (B, A)=>B): B = foldMap(as.map(x => {b:B => f.curried(b)(x)}), endoMonoid[B])(identity)(z)

    def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
      def op(a1:List[A], a2:List[A]): List[A] = a1 ++ a2

      def zero: List[A] = Nil
    }

    val intAddition: Monoid[Int] = new Monoid[Int] {
      def op(a1:Int, a2:Int): Int = a1 + a2

      def zero: Int = 0
    }

    val intMultiplication: Monoid[Int] = new Monoid[Int] {
      def op(a1:Int, a2:Int): Int = a1 * a2

      def zero: Int = 1
    }

    val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
      def op(a1: Boolean, a2:Boolean): Boolean = a1 || a2

      def zero: Boolean = false
    }

    val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
      def op(a1: Boolean, a2:Boolean): Boolean = a1 && a2

      def zero: Boolean = true
    }

    def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
      def op(a1: Option[A], a2:Option[A]): Option[A] = (a1, a2) match {
        case (None, None) => None
        case (Some(_), Some(_)) => a1
      }

      def zero: Option[A] = None
    }

    def endoMonoid[A]: Monoid[A=>A] = new Monoid[A=>A] {
      def op(a1:A=>A, a2:A=>A): A=>A = a => a2(a1(a))

      def zero = identity
    }
  }
}
