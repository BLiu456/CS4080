import scala.io.StdIn.readLine
import scala.io.StdIn.readInt
import scala.io.StdIn.readDouble

def main_menu: Int =
  print(
    "---Calculator---\n" +
      "1. Add\n" +
      "2. Subtract\n" +
      "3. Multiplication\n" +
      "4. Division\n" +
      "5. Calculate a shapes area\n" +
      "6. Exit\n" +
      "Enter: "
  )
  readInt()

def adder(): Unit =
  print("Enter first value: ")
  val x = readDouble()
  print("Enter second value: ")
  val y = readDouble()
  println(f"$x + $y = ${x + y}")

def subtractor(): Unit =
  print("Hi")

@main def main: Unit =
  var looping = true
  while looping do
    main_menu match
      case 1 => adder()
      case 2 => ???
      case 3 => ???
      case 4 => ???
      case 5 => ???
      case 6 => looping = false
