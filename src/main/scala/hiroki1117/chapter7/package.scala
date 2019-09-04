package hiroki1117

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

package object chapter7 {

  type Par[A] = ExecutorService => Future[A]

  object Par {
    def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)
    def lazyUnit[A](a: A): Par[A] = fork(unit(a))

    def map[A,B](a: Par[A])(f: A=>B): Par[B] = map2timeoutable(a, unit(()))((a,_) => f(a))

    private case class UnitFuture[A](get: A) extends Future[A] {
      override def isDone: Boolean = true
      override def get(timeout: Long, unit: TimeUnit): A = get
      override def isCancelled: Boolean = false
      override def cancel(mayInterruptIfRunning: Boolean): Boolean = false
    }

    def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B)=>C): Par[C] =
      (es: ExecutorService) => {
        val af = a(es)
        val bf = b(es)

        //getにtimeoutを指定していないのでmap2はタイムアウトを制御できない
        UnitFuture(f(af.get, bf.get))
      }

    def map3[A,B,C,D](a: Par[A],b: Par[B],c: Par[C])(f: (A,B,C) => D): Par[D] = map2(map2(a,b)(f.curried(_)(_)), c)(_(_))
    //map2(map2(a,b)((_,_)),c){case ((a1,b1),c1) => f(a1,b1,c1)}

    //外側のCallableがスレッドで実行される時、もう一つのスレッドでaが実行される
    //forkは最低でも2つのスレッドを使用する事になる
    //デッドロックの原因になる
    def fork[A](a: => Par[A]): Par[A] =
      es => es.submit(new Callable[A] {
        def call = a(es).get
      })

    def delay[A](fa: => Par[A]): Par[A] = es => fa(es)

    def asyncF[A,B](f: A=>B): A=>Par[B] = a => lazyUnit(f(a))

    def parMap[A,B](ps:List[A])(f:A=>B):Par[List[B]] = fork{
      val fbs: List[Par[B]] = ps.map(asyncF(f))
      sequence(fbs)
    }

    def parFilter[A](l: List[A])(f: A=>Boolean): Par[List[A]] = {
      val pars: List[Par[List[A]]] = l map (asyncF(a => if (f(a)) List(a) else List()))
      map(sequence(pars))(_.flatten)
    }

    def sequence[A](ps: List[Par[A]]): Par[List[A]] =
      ps.foldRight(Par.unit(Nil): Par[List[A]])((e, acc)=> map2(e, acc)(_::_))

    def map2timeoutable[A,B,C](a: Par[A],b: Par[B])(f: (A,B)=>C): Par[C] =
      es => {
        val (af, bf) = (a(es), b(es))
        Map2Future(af, bf, f)
      }

    private case class Map2Future[A,B,C](a: Future[A], b: Future[B],
                                         f: (A,B) => C) extends Future[C] {
      //計算結果を保持する
      @volatile var cache: Option[C] = None
      //計算結果が存在するかどうか
      def isDone = cache.isDefined
      def isCancelled = a.isCancelled || b.isCancelled
      def cancel(mayInterruptIfRunning: Boolean): Boolean =
        a.cancel(mayInterruptIfRunning) || b.cancel(mayInterruptIfRunning)
      def get = compute(Long.MaxValue)
      // NANOSECONDSに変換
      def get(timeout: Long, units: TimeUnit): C =
        compute(TimeUnit.NANOSECONDS.convert(timeout, units))



      //a,bを時間を測りながら実行する
      private def compute(timeoutInNanos: Long): C = cache match {
        case Some(c) => c
        case None =>
          val start = System.nanoTime()
          val ar = a.get(timeoutInNanos, TimeUnit.NANOSECONDS)
          val stop = System.nanoTime()
          val aTime = stop-start
          val br = b.get(timeoutInNanos-aTime, TimeUnit.NANOSECONDS)
          val ret = f(ar, br)
          cache = Some(ret)
          ret
      }
    }

    def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = p(e).get == p2(e).get()

    def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)

    def parMax(ints: IndexedSeq[Int]): Par[Int] = {
      if(ints.size<=1) {
        Par.unit(ints.headOption.getOrElse(0))
      } else {
        val (l,r) = ints.splitAt(ints.length/2)
        Par.map2(fork(parMax(l)), fork(parMax(r)))((ll, rr) => if(ll>=rr) rr else ll)
      }
    }

    def countEachWord(sentence: List[String]): Par[List[Int]] = countSentence(sentence)(_ split "" length)

    def countSentence(sentence: List[String])(countF: String=>Int): Par[List[Int]] = sequence(sentence.map(asyncF(countF)))
  }
}
