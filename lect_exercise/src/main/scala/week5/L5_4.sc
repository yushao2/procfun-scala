// Map/filter/reduce on list

// using rec & pattern match
def squareList(xs: List[Int]): List[Int] = xs match {
    case Nil => Nil
    case y :: ys => (y*y) :: squareList(ys)
  }
// map
def squareListM(xs: List[Int]): List[Int] =
  xs map (p => p*p)


def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 =>
    val (first, rest) = xs span (y => y == x)
    first :: pack(rest)
}

def encode[T](xs: List[T]): List[(T, Int)] = {
  pack(xs) map (p=> (p.head, p.length))
}

val data = "aaabcca".toList
encode(data)
