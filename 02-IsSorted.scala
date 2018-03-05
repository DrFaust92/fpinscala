def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {

  def go(as: Array[A]): Boolean =
    if (as.isEmpty) true
    else if (as.tail.isEmpty) true
    else if (gt(as.head, as.tail.head)) false
    else go(as.tail)

  go(as)

}

isSorted[Int](Array(), (d, s) => d > s)
isSorted[Int](Array(5), (d, s) => d > s)
isSorted[Int](Array(5,4), (d, s) => d > s)
isSorted[Int](Array(4,5), (d, s) => d > s)
