object Numerical_Differentiation {
  
  class Cons[T](h: => T, t: => Cons[T]) {
    val fst: T = h
    def rem: Cons[T] = t
  }

  object Cons {
    def apply[T](h: T, rem: => Cons[T]): Cons[T] = {
      new Cons(h, rem)
    }

    def unapply[T](arg: Cons[T]): Option[(T, Cons[T])] = {
      Some(arg.fst, arg.rem)
    }
  }

  def get_snd_fst(list: Cons[Double]) = list.rem.fst

  def repeat[A](f: A => A, a: A): Cons[A] = Cons(a, repeat(f, f(a)))

  def within(eps: Double, list: Cons[Double]): Double = {
    if (Math.abs(list.fst - list.rem.fst) <= eps) {
      list.rem.fst
    } else {
      within(eps, list.rem)
    }
  }

  def map[A, B](f: A => B)(list: Cons[A]): Cons[B] = list match {
    case c: Cons[A] => Cons(f(c.fst), map(f)(c.rem))
  }

  def halve(x: Double): Double = x / 2
  
  def order(list: Cons[Double]) = list match {
    case Cons(a, Cons(b, Cons(c, _))) =>
      val res = Math.round(Math.log((a - c) / (b - c) - 1))
      if (res.isNaN || res == 0) 1L else res
  }

  def easydiff(f: Double => Double, x: Double)(h: Double): Double = (f(x + h) - f(x)) / h

  def diff(h0: Double, f: Double => Double, x: Double) = map(easydiff(f, x))(repeat(halve, h0))

  def elimerror(n: Double)(list: Cons[Double]): Cons[Double] = list match {
    case Cons(a, Cons(b, rest)) => Cons(b * Math.pow(2, n) - a / (Math.pow(2, n) - 1), elimerror(n)(Cons(b, rest)))
  }

  def improve(s: Cons[Double]) = elimerror(order(s))(s)
  def improve_super(s: Cons[Double]) = map(get_snd_fst)(repeat(improve, s))

  
  def differentiate(eps: Double, h0: Double, f: Double => Double, x: Double) = within(eps, diff(h0, f, x))
  def differentiate_improve(eps: Double, h0: Double, f: Double => Double, x: Double) = within(eps, improve(diff(h0, f, x)))
  def differentiate_super(eps: Double, h0: Double, f: Double => Double, x: Double) = within(eps, improve_super(diff(h0, f, x)))

  def main(args: Array[String]): Unit = {
    println(s"*****************************")
    println(s"* Numerical_Differentiation *")
    println(s"*     ID: 20210240108       *")
    println(s"*****************************")
    println(s"")
    println(s"The result of differentiating x^2 at point x=2 is (eps set to 0.001):")
    println(s"")

    val res1 = differentiate(0.001, 1, x => x * x, 2)
    val res2 = differentiate_improve(0.001, 1, x => x * x, 2)
    val res3 = differentiate_super(0.001, 1, x => x * x, 2)

    println(s"1. differentiate:\t\t$res1")
    println(s"2. differentiate_improve:\t$res2")
    println(s"3. differentiate_super:\t\t$res3")
  }
}