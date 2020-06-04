val n = 7

def isPrime(n: Int) = (2 until n) forall (n % _ != 0)

(1 until n) flatMap ( i => (1 until i) map (j => (i,j))) filter (pair => isPrime(pair._1 + pair._2))

// For-Expressions

// for (s) yield e

for {
  i <- 1 until n
  j <- 1 until i
  if isPrime(i+j)
} yield (i, j)

def scalarProduct(xs: List[Double], ys: List[Double]) : Double =
  (for ((x,y) <- xs zip ys) yield x * y).sum
