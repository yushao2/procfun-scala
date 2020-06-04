import scala.math

def isSafe(col: Int, queen: List[Int]): Boolean = {
  val row = queen.length
  val queensWithRow = (row-1 to 0 by -1) zip queen
  queensWithRow forall {
    case (r,c) => col != c && math.abs(col-c) != (row - r)
  }
}


def queens(n: Int): Set[List[Int]] = {
  def placeQueens(k:Int): Set[List[Int]] =
    if (k == 0) Set(List())
    else
      for {
        queens <- placeQueens(k - 1)
        col <- 0 until n
        if isSafe(col, queens)
      }yield col :: queens
  placeQueens(n)
}

queens(4)