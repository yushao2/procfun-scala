// Traits
// Classes sometimes have several natural supertypes to which it conforms or want to inherit code from.
// Traits can be used

//Traits are declared like abstract classes.

trait Planar {
  def height: Int
  def width: Int
  def surface = height * width
}

// Classes, objects and traits can inherit from at most 1 class, but arbitarary many traits.
// Traits resemble interfaces in Java, but are more powerful -- they can contain fields and concrete methods.
// Traits cannot have value parameters -- only classes can.

if (true) 1 else false // resultant type is AnyVal -> mismatch so matched to parent type

val x = null
val y: String = null
val z: Int = null // error

