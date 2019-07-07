package hiroki1117

package object chapter6 {
  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }


  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (Int.MinValue, r) => nonNegativeInt(r)
    case (i, r) => (Math.abs(i), r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (num, next) = nonNegativeInt(rng)
    (num.toDouble/Int.MaxValue, next)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, next) = nonNegativeInt(rng)
    val (d, next2) = double(next)
    ((i,d), next2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), next) = intDouble(rng)
    ((d,i), next)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, next) = double(rng)
    val (d2, next2) = double(next)
    val (d3,next3) = double(next2)
    ((d1,d2,d3), next3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if(count<=0) (Nil, rng)
    else {
      var next: RNG = rng
      val result = for (_ <- 1 to count) yield {
        val (int, r) = next.nextInt
        next = r
        int
      }
      (result.toList, next)
    }

  type Rand[+A] = RNG => (A,RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A=>B): Rand[B] =
    rng => {
      val (a, next) = s(rng)
      (f(a), next)
    }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i=>i-i%2)

  def doubleByMap: Rand[Double] = map(nonNegativeInt)(i => i.toDouble/Int.MaxValue)

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B)=>C): Rand[C] =
    rng => {
      val (a, next) = ra(rng)
      val (b, next2) = rb(next)
      (f(a,b), next2)
    }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra,rb)((_, _))

  def randIntDouble: Rand[(Int,Double)] = both(int, double)

  def randDouble: Rand[(Double,Int)] = both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(Nil): Rand[List[A]]){(e, acc) => map2(e, acc)(_::_)}

  def ints(counts: Int): Rand[List[Int]] = sequence(List.fill(counts)(int))

  def flatMap[A,B](f: Rand[A])(g: A=>Rand[B]): Rand[B] =
    rng => {
      val (a, next) = f(rng)
      g(a)(next)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt) { i =>
    val mod = i%n
    if(i+(n-1)-mod>=0) unit(mod) else nonNegativeLessThan(n)
  }

  def mapByFlat[A,B](f: Rand[A])(g:A=>B):Rand[B] = flatMap(f)(x=>unit(g(x)))

  def map2ByFlat[A,B,C](fa:Rand[A], fb:Rand[B])(f:(A,B)=>C):Rand[C] = flatMap(fa)(a => map(fb)(b=>f(a,b)))

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_+1)

  case class State[S, +A](run: S=>(A,S))
}
