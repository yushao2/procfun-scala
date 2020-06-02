def product(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) 1
  else f(a) * product(f)(a+1, b)

def fact(n:Int): Int =
  product(x => x)(1,n)


fact(5)


//More generic function: MapReduce
def mapReduce(f: Int => Int, combine: (Int,Int) => Int, zero: Int)(a: Int, b: Int): Int =
  if (a > b) zero
  else combine(f(a), mapReduce(f, combine, zero)(a+1, b))

def productMR(f: Int => Int)(a:Int, b:Int):Int =
  mapReduce(f, (x,y)=>x*y, 1)(a,b)

def factMR(n:Int): Int =
  productMR(x => x)(1,n)

factMR(5)