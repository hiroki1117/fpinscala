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
      var next: RNG
      val result = for (_ <- 1 to count) yield {
        val (int, r) = rng.nextInt
        next = r
        int
      }
      (result.toList, next)
    }
}
