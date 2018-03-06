sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r)
  }

  def max(tree: Tree[Int]): Int = tree match {
    case Leaf(v) => v
    case Branch(l, r) => max(l).max(max(r))
  }


  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }


  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](t: Tree[A])(g: A => B)(b: (B, B) => B): B = t match {
    case Leaf(v) => g(Leaf(v))
    case Branch(l, r) => b(fold(l)(g), fold(r)(g))
  }


  def size2[A](tree: Tree[A]): Int = fold(tree)(_ => 1)(1 + _ + _)

  def max2(tree: Tree[Int]): Int = fold(tree)(a => a)((x, y) => x.max(y))


  def depth2[A](tree: Tree[A]): Int = fold(tree)(_ => 0)((x, y) => 1 + x.max(y))


  def map2[A, B](tree: Tree[A])(f: A => B): Tree[B] = fold(tree)(a => Leaf(f(a)): Tree[B])(Branch(_, _))

}

