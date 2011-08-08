class Monad[A](v: A) {
  def flatMap[B](f: A => Monad[B]): Monad[B] = f(v)
  def map[B](f: A => B): Monad[B] = new Monad(f(v))
  override def toString = "(" + v.toString + ")"
}

trait Monader[C[_]] {
  def unit[A](a: A): C[A]

  def map[A, B](ma: C[A])(f: A => B): C[B]

  def flatMap[A, B](ma: C[A])(f: A => C[B]): C[B]
}

object ListMonad extends Monader[List] {
  def unit[A](a: A): List[A] = List(a)

  def map[A, B](la: List[A])(f: A => B) = la map f

  def flatMap[A, B](la: List[A])(f: A => List[B]) = la flatMap f
}

sealed trait MyOption[+A] {
  def flatMap[B](f: A => MyOption[B]): MyOption[B]
  def map[B](f: A => B): MyOption[B]
}

case class MySome[+A](v: A) extends MyOption[A] {
  def flatMap[B](f: A => MyOption[B]): MyOption[B] = f(v)
  def map[B](f: A => B): MyOption[B] = new MySome(f(v))
}

case object MyNone extends MyOption[Nothing] {
  def flatMap[B](f: Nothing => MyOption[B]): MyOption[B] = MyNone
  def map[B](f: Nothing => B): MyOption[B] = MyNone
}

object Run {
  def main(args: Array[String]) {
    val a = new MySome("hihi")
    val b = MyNone
    val c = new MySome("haha")
    val f: (MyOption[String] => String) = (_ match {
      case MyNone => "Dans l'cul lulu!"
      case MySome(s) => s
    })
    a :: b :: Nil map (s => println(f(s)))
    println(a flatMap (aa => c map (cc => aa + cc)))
    println(a flatMap (aa => b map (bb => aa + bb)))
    println(for (aa <- a; cc <- c) yield aa + cc)
    println(for (aa <- a; bb <- b) yield aa + bb)

    val la = ListMonad.unit("two")
    println(la)
    val la2 = la map { "one" + _ } map { _ + "three"}
    println(la2)
    //val moli = new Monader[List] {
      //def unit[A](a: Int) = List(a)
      //def map[A, B](a: Int, f: Int => List[Int]) = f(a)
      //def flatMap[A, B](a: Int, f: Int => Int) = List(f(a))
    //}
  }
}
