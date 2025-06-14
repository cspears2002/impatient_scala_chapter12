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

// Exercise 3
def computeFactorial(startingInt: Int): Int = {
  if startingInt == 0 then
    1
  else {
    val i = startingInt - 1
    (i to 1).foldLeft(startingInt)(_ * _)
  }
}
computeFactorial(0)
computeFactorial(5)
