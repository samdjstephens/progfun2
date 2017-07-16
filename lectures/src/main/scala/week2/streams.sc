def streamRange(lo: Int, hi: Int): Stream[Int] = {
  println(s"$lo ")
  if (lo >= hi) Stream.empty
  else Stream.cons(lo, streamRange(lo + 1, hi))
}

streamRange(1, 10).take(3).toList


def from(n: Int): Stream[Int] = n #:: from(n+1)


val nats = from(0)
val m4s = nats map (_ * 4)

m4s.takeWhile(_ < 100).toList

nats map (_ * 4)


def sqrtStream(x: Double): Stream[Double] = {
  def improve(guess: Double) = (guess + x / guess) / 2
  lazy val guesses: Stream[Double] = 1 #:: (guesses map improve)
  guesses
}
