sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(x, xs) => x * product(xs)
  }

  def tail[A](as: List[A]): List[A] = as match {
    case Nil => throw new UnsupportedOperationException
    case Cons(_, xs) => xs
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] =
    l match {
      case Cons(h, t) if f(h) => dropWhile(t)(f)
      case _ => l
    }


  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => throw new UnsupportedOperationException
    case Cons(_, xs) => Cons(h, xs)
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new UnsupportedOperationException
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }


  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }


  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }


  def length[A](l: List[A]): Int = foldRight(l, 0)((_, y) => y + 1)


  def sum2(l: List[Int]) =
    foldRight(l, 0.0)(_ + _)


  def product2(l: List[Double]) =
    foldRight(l, 1.0)(_ * _)


  def sum3(l: List[Int]) =
    foldLeft(l, 0.0)(_ + _)


  def product3(l: List[Double]) =
    foldLeft(l, 1.0)(_ * _)


  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))


  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc, h) => Cons(h, acc))


  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }


  def concat[A](l: List[List[A]]): List[A] = foldRight(l, List[A]())(append)


  def map[A, B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil: List[B])((h, acc) => Cons(f(h), acc))


  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = concat(map(l)(f))

  def filter[A](l: List[A])(f: A => Boolean): List[A] = foldRight(l, List[A]())((h, acc) => if (f(h)) Cons(h, acc) else acc)

  def filter2[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l)(h => if (f(h)) Cons(h, Nil) else Nil)

  //def op[A](a1: List[A], a2: List[A])(f: A => A): List[A] =

  def addition[A](a1: List[A], a2: List[A])(f: (A, A) => A): List[A] = a1 match {
    case Nil => a2
    case Cons(x, xs) => a2 match {
      case Nil => a1
      case Cons(y, ys) => Cons(f(x, y), addition(xs, ys)(f))
    }
  }


  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = (l,sub) match {
    case (_,Nil) => true
    case (Nil, _) => false
    case (Cons(x,xs),Cons(y,ys)) if x == y => hasSubsequence(xs,ys)
    case (Cons(_,xs),sb) => hasSubsequence(xs,sb)
  }


}


val u = List()
val g = List(1, 2, 3, 4, 5)
val k = List(2,3)
val z = List.reverse(List(1, 2, 3, 4, 5))

List.dropWhile(g)(x => x < 3)
//List.init(x)

List.map(g)(x => x + 1)

List.filter2(g)((y: Int) => y % 2 == 0)



List.addition(g, z)((a, b) => a + b)
List.addition(g, z)((a, b) => a * b)


List.hasSubsequence(g,g)
List.hasSubsequence(g,u)
List.hasSubsequence(g,k)
List.hasSubsequence(g,List(4))

List.hasSubsequence(g,List(0))




