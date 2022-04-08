object A:
  def foo[T](x: T, array: Array[T]): Unit = array(0) = x

class B {
  var a = Array.apply(1, 2, 3)
  A.foo(this.i, a)
  val i = 99
}

val b = new B()