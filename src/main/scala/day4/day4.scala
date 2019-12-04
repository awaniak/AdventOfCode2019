package day4

object day4 extends App {

  def isOrdered(arrayOfDigits: List[Int]): Boolean = {
    arrayOfDigits match {
      case Nil => true
      case _ :: Nil => true
      case h :: t => if (h <= t.head) isOrdered(t) else false
    }
  }

  def areThereTwoAdjacentDigits(arrayOfDigits: List[Int], alreadyAdjacentDigits: Int = 0): Boolean = {
    (arrayOfDigits, alreadyAdjacentDigits) match {
      case (Nil, _) => false
      case (_ :: Nil, numOfAdjacent) => if (numOfAdjacent == 2) true else false
      case (h :: t, 0) =>
        if (h == t.head) areThereTwoAdjacentDigits(t, 2) else areThereTwoAdjacentDigits(t)
      case (h :: t, numOfAdjacent) =>
        if (h == t.head) areThereTwoAdjacentDigits(t, numOfAdjacent + 1) else {
        if (numOfAdjacent == 2) true else areThereTwoAdjacentDigits(t)
      }
    }
  }

  def getNumOfPasswords(a: Int, b: Int): Int = {
    var counter = 0;
    for (i <- a to b) {
      val arrayOfDigits: List[Int] = i.toString.map(_.asDigit).toList
      if (isOrdered(arrayOfDigits) && areThereTwoAdjacentDigits(arrayOfDigits)) {
        counter = counter + 1
      }
    }
    counter;
  }

  println("Number of passwords meeting criteria: " + getNumOfPasswords(402328, 864247))
}
