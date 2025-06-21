// Exercise 1
import scala.collection.mutable.ArrayBuffer

def values(f: Int => Int, low: Int, high: Int): ArrayBuffer[(Int, Int)] = {
  val arrayBuff = ArrayBuffer[(Int, Int)]()
  for (i <- low until high + 1) {
    arrayBuff += ((i, f(i)))
  }
  arrayBuff
}
values(x => x * x, -5, 5)

// Exercise 2
val intArray = Array(5, 6, 10, 9, 1)
val biggestInt: Int = intArray.foldLeft(0) { (x, y) =>
  if x > y then x else y
}

// Exercise 3 and 4
def computeFactorial(n: Int): Option[Int] =
  Option.when(n >= 0) { 1.to(n).foldLeft(1)(_ * _) }
assert(computeFactorial(-1).isEmpty)
assert(computeFactorial(0).contains(1))
assert(computeFactorial(5).contains(120))

