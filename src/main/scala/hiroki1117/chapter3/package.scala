package hiroki1117

package object chapter3 {
  sealed trait List[+A]{

    def headOption: Option[A] = this match {
      case Cons(h, _) => Some(h)
      case Nil => None
    }

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

    def foldRightByFoldLeft[B](z: B)(f: (A,B)=>B): B = this.reverse.foldLeft(z)((b,a)=>f(a,b))

    def length: Int = foldRight(0)((_,acc) => 1 + acc)

    def sum[B >: A :Numeric]: B = foldLeft(implicitly[Numeric[B]].zero)(implicitly[Numeric[B]].plus(_, _))

    def product[B >: A : Numeric]: B = foldLeft(implicitly[Numeric[B]].one)(implicitly[Numeric[B]].times(_,_))

    def lengthByFoldLeft = foldLeft(0)((acc, _) => 1 + acc)

    def reverse: List[A] = foldLeft(Nil: List[A])((acc, e) => Cons(e, acc))

    def append[B >: A](other: List[B]): List[B] = foldRight(other)((ele, acc)=> Cons(ele, acc))

    def flat[B](implicit ev: A <:< List[B]): List[B] = foldRight(Nil: List[B])((ele, acc) => ele.append(acc))

    def addOne(implicit ev: A <:< Int): List[Int] = this match {
      case Cons(h, t) =>  Cons(h + 1, t.addOne)
      case Nil => Nil
    }

    def doubleList2StringList(implicit ev: A <:< Double): List[String] = this match {
      case Cons(h,t) => Cons(h.toString, t.doubleList2StringList)
      case Nil => Nil
    }

    def map[B](f: A=>B): List[B] = this match {
      case Cons(h,t) => Cons(f(h), t.map(f))
      case Nil => Nil
    }

    def filter(f: A=>Boolean): List[A] = this match {
      case Cons(h,t) => if(f(h)) t.filter(f) else Cons(h, t.filter(f))
      case Nil => Nil
    }

    def removeOdd(implicit ev: A <:< Int): List[A] = this.filter(_%2==1)

    def flatMap[B](f:A=>List[B]): List[B] = this match {
      case Cons(h,t) => f(h).append(t.flatMap((f)))
      case Nil => Nil
    }
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

    def addOne(list: List[Int]): List[Int] = list match {
      case Cons(h, t) => Cons(h + 1, addOne(t))
      case Nil => Nil
    }

    def doubleToStringList(list: List[Double]): List[String] = list match {
      case Cons(h,t) => Cons(h.toString, doubleToStringList(t))
      case Nil => Nil
    }

    def map[A,B](as: List[A])(f:A=>B):List[B] = as match {
      case Cons(h,t) => Cons(f(h), map(t)(f))
      case Nil => Nil
    }

    def filter[A](as: List[A])(f: A=>Boolean): List[A] = as match {
      case Cons(h,t) => if(f(h)) filter(t)(f) else Cons(h, filter(t)(f))
      case Nil => Nil
    }

    def removeOdd(list: List[Int]): List[Int] = filter(list)(_%2==1)

    def flatMap[A,B](as:List[A])(f: A=>List[B]): List[B] = flat(map(as)(f))

    def filterByFlatMap[A](as: List[A])(f: A=>Boolean): List[A] = flatMap(as)(x => if(f(x)) Nil else List(x))

    def zip(l1: List[Int], l2: List[Int]): List[Int] = map(zipWith(l1, l2))(tuple => tuple._1 + tuple._2)

    def zipWith[A, B](l1: List[A], l2: List[B]): List[(A,B)] = (l1, l2) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(a,b), Cons(c,d)) => Cons((a,c), zipWith(b, d))
    }

    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
      def go[B](n: Int, list: List[B] ,acc: List[List[B]]): List[List[B]] = if(n > list.length) acc else
        append(Cons(take(list, n), acc), go(n+1, list, acc))

      def go2(list: List[A], acc: List[List[A]]): List[List[A]] = list match {
        case Nil => Nil
        case Cons(_, t) => append(go(1, list, Nil), go2(t, Nil))
      }
      val sublist = go2(sup, Nil)
      foldLeft(sublist, false)((acc, e)=> acc || (e==sub))
    }

    def take[A](list:List[A], n:Int): List[A] = if(n<=0) Nil else list match {
      case Nil => Nil
      case Cons(h, t) => Cons(h, take(t, n-1))
    }
  }

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {

    def apply[A](a: A): Tree[A] = Leaf(a)

    def branch[A](l:Tree[A], r:Tree[A]): Tree[A] = Branch(l,r)

    def size[A](tree: Tree[A]): Int = tree match {
      case Leaf(_) => 1
      case Branch(l, r) => size(l) + size(r)
    }

    def maximum[A: Ordering](tree: Tree[A]): A = tree match {
      case Leaf(v) => v
      case Branch(l, r) => implicitly[Ordering[A]].max(maximum(l), maximum(r))
    }

    def depth[A](tree: Tree[A]): Int = tree match {
      case Leaf(_) => 0
      case Branch(l, r) => (1+depth(l)) max (1+depth((r)))
    }

    def map[A,B](tree: Tree[A])(f: A=>B):Tree[B] = tree match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l,r) => Branch(map(l)(f), map(r)(f))
    }

    def fold[A, B](tree: Tree[A])(f: A => B)(op: (B, B) => B): B = tree match {
      case Leaf(v) => f(v)
      case Branch(l, r) => op(fold(l)(f)(op), fold(r)(f)(op))
    }

    def size2[A](tree:Tree[A]): Int = fold(tree)(_ => 1)(_ + _)

    def maximum2[A: Ordering](tree:Tree[A]): A = fold(tree)(identity)(implicitly[Ordering[A]].max(_, _))

    def depth2[A](tree:Tree[A]): Int = fold(tree)(_=>1)(_ max _)

    def map2[A,B](tree:Tree[A])(f:A=>B):Tree[B] = fold(tree)(v=>Tree(f(v)))((l,r)=>Tree.branch(l, r))

  }
}
