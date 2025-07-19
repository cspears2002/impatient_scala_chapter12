// Exercise 1
import scala.collection.mutable.ArrayBuffer
import scala.math.multiplyExact

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

// Exercise 5
def myFunc(x: Int): Int = 10 * x - x * x
val mySeq: Seq[Int] = 1 to 10
mySeq.map(myFunc)
def largest(fun: Int => Int, inputs: Seq[Int]): Int =
  inputs.map(fun).max
largest(myFunc, mySeq)

// Exercise 6
def idxForLargestVal(fun: Int => Int, inputs: Seq[Int]): Int = {
  val newSeq = inputs.map(fun)
  val maxVal = newSeq.max
  newSeq.indexOf(maxVal) + 1
}
idxForLargestVal(myFunc, mySeq)

// Exercise 7
import scala.math.sqrt

def f(x: Double) = if x != 1 then Some(1 / (x - 1)) else None
def g(x: Double) = if x >= 0 then Some(sqrt(x)) else None
def composeFunc(f: Double => Option[Double],
                g: Double => Option[Double]): Double => Option[Double] =
  x => f(x).flatMap(g)
val h = composeFunc(f, g)
h(2)
h(1)
h(0)

// Exercise 8
def composeIntFunc(f: Int => Int, g: Int => Int): Int => Int =
  x => f(g(x))
val double: Int => Int = _ * 2
val triple: Int => Int = _ * 3
val doubleTriple = composeIntFunc(triple, double)
doubleTriple(2)

def tupledIntFunc(f: (Int, Int) => Int): ((Int, Int)) => Int =
  (x: Int, y: Int) => f(x, y)
tupledIntFunc(multiplyExact)((6, 7))

def curriedIntFunc(f: (Int, Int) => Int): Int => Int => Int =
  x => (y: Int) => f(x,y)

val intMax: (Int, Int) => Int = scala.math.max
val maxZero = curriedIntFunc(intMax)
maxZero(0)(1)
maxZero(0)(-1)

// Exercise 10
val a = Array("Mary", "had", "a", "little", "lamb")
val b = Array(4, 3, 1, 6, 4)
a.corresponds(b)(_.length == _)

// Exercise 11
def correspondsArrays(arrayA: Array[String],
                      arrayB: Array[Int],
                      p: (String, Int) => Boolean): Boolean = {
  arrayA.zip(arrayB).map(p(_,_)).forall(_ == true)
}
val lengthFunc: (String, Int) => Boolean = (x: String, y: Int) => x.length == y
correspondsArrays(a, b, lengthFunc)

// Exercise 12
def unless(condition: => Boolean)(block: => Unit): Unit =
  if !condition then
    block

val x = 2
unless (x == 1) {
  println(x)
}

