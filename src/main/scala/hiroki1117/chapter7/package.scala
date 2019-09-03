package hiroki1117

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

package object chapter7 {

  type Par[A] = ExecutorService => Future[A]

  object Par {
    def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

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

    //外側のCallableがスレッドで実行される時、もう一つのスレッドでaが実行される
    //forkは最低でも2つのスレッドを使用する事になる
    //デッドロックの原因になる
    def fork[A](a: => Par[A]): Par[A] =
      es => es.submit(new Callable[A] {
        def call = a(es).get
      })

    def map2timeoutable[A,B,C](a: Par[A],b: Par[B])(f: (A,B)=>C): Par[C] =
      es => {
        val (af, bf) = (a(es), b(es))

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
  }
}
