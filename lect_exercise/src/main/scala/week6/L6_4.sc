

class Poly(val terms0: Map[Int, Double]) {
  def this(bindings: (Int, Double)*) = this(bindings.toMap) // multiple bindings
  val terms = terms0 withDefaultValue 0.0
  def + (other: Poly) = new Poly(terms ++ other.terms map adjust)
  def adjust(term: (Int, Double)): (Int, Double) = {
    val (exp,coeff) = term
    exp -> (coeff + terms(exp))
  }
  override def toString =
    (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff + "x^"+exp) mkString " + "
}