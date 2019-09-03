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

    def fork[A](a: => Par[A]): Par[A] =
      es => es.submit(new Callable[A] {
        def call = a(es).get
      })
  }
}
