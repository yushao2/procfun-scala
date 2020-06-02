import java.util.NoSuchElementException

// Class hierarchy
// [T] -- type parameter
trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

// "val" parameters in class definition are automatically initialised
class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty: Boolean = false
}

class Nil[T] extends List[T] {
  def isEmpty: Boolean = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

// Functions can have type params
def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])
singleton[Int](1)
singleton[Boolean](true)
// Type can be inferred
singleton(1)

// Exercise: Write a function to return nth element of list
@scala.annotation.tailrec
def nth [T](n: Int, l: List[T]): T = {
  if (l.isEmpty) throw new IndexOutOfBoundsException
  else if (n == 0) l.head
  else nth(n-1, l.tail)
}

val list = new Cons(1, new Cons(2, new Cons (3, new Nil)))

nth(-3, list)