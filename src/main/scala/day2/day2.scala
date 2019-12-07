object Day2 extends App {
  val ADD_VALUE = 1
  val MULTIPLICATION_VALUE = 2
  val END_PROGRAM_VALUE = 99
  val OFFSET = 4

  def stringToIntList(input: String): List[Int] = input.split(",").map(_.toInt).toList

  def runProgram(intcode: List[Int], noun: Int, verb: Int): Int = {
    val memory = intcode.toBuffer
    memory(1) = noun
    memory(2) = verb
    var instructionPointer = 0
    while (memory(instructionPointer) != END_PROGRAM_VALUE) {
      val instruction: Int = memory(instructionPointer)
      val firstParam: Int = memory(memory(instructionPointer + 1))
      val secParam: Int = memory(memory(instructionPointer + 2))
      val outputAddress: Int = memory(instructionPointer + 3)
      instruction match {
        case ADD_VALUE => memory(outputAddress) = firstParam + secParam
        case MULTIPLICATION_VALUE => memory(outputAddress) = firstParam * secParam
      }
      instructionPointer = instructionPointer + OFFSET
    }
    memory.head
  }

  val input = "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,6,1,19,1,19,5,23,2,9,23,27,1,5,27,31,1,5,31,35,1,35,13,39,1,39,9,43,1,5,43,47,1,47,6,51,1,51,13,55,1,55,9,59,1,59,13,63,2,63,13,67,1,67,10,71,1,71,6,75,2,10,75,79,2,10,79,83,1,5,83,87,2,6,87,91,1,91,6,95,1,95,13,99,2,99,13,103,1,103,9,107,1,10,107,111,2,111,13,115,1,10,115,119,1,10,119,123,2,13,123,127,2,6,127,131,1,13,131,135,1,135,2,139,1,139,6,0,99,2,0,14,0"
  val initMemory = stringToIntList(input)

  println("#######################################################")
  println("Task 1")
  println("Result: " + runProgram(initMemory, 12, 2))
  println("#######################################################")
  println("Task 2")
  for (i <- 0 to 99) {
    for (j <- 0 to 99) {
      if (runProgram(initMemory, i, j) == 19690720) {
        println(s"Found answer, noun: $i, verb: $j")
        println("100 * noun + verb = " + (100 * i + j))
      }
    }
  }
  println("#######################################################")

}