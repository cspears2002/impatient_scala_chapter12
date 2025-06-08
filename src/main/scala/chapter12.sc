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


