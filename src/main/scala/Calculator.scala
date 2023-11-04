import scala.io.StdIn.readLine
import scala.io.StdIn.readInt
import scala.io.StdIn.readDouble
import scala.math._

trait Shape:
  def calcArea(): Double

case class Triangle(base: Double, height: Double) extends Shape:
  def calcArea(): Double =
    (base * height) / 2

object Triangle: // Companion object used to define apply method
  def apply() =
    print("Enter base of triangle: ")
    val base = readDouble()
    print("Enter height of triangle: ")
    val height = readDouble()
    new Triangle(base, height)

case class Rectangle(length: Double, height: Double) extends Shape:
  def calcArea(): Double =
    length * height

object Rectangle:
  def apply() =
    print("Enter length pf Rectangle: ")
    val length = readDouble()
    print("Enter height of triangle: ")
    val height = readDouble()
    new Rectangle(length, height)

case class Pentagon(side: Double) extends Shape:
  def calcArea(): Double =
    val factor = 0.25 * sqrt(5 * (5 + (2 * sqrt(5))))
    factor * pow(side, 2)

object Pentagon:
  def apply() =
    print("Enter side length of pentagon: ")
    val side = readDouble()
    new Pentagon(side)

case class Hexagon(side: Double) extends Shape:
  def calcArea(): Double =
    val factor = (3 * sqrt(3)) / 2
    factor * pow(side, 2)

object Hexagon:
  def apply() =
    print("Enter side length of hexagon: ")
    val side = readDouble()
    new Hexagon(side)

case class Octagon(side: Double) extends Shape:
  def calcArea(): Double =
    val factor = 2 * (1 + sqrt(2))
    factor * pow(side, 2)

object Octagon:
  def apply() =
    print("Enter side length of octagon: ")
    val side = readDouble()
    new Octagon(side)

case class Circle(radius: Double) extends Shape:
  def calcArea(): Double =
    Pi * pow(radius, 2)

object Circle:
  def apply() =
    print("Enter radius of circle: ")
    val radius = readDouble()
    new Circle(radius)

def main_menu: Int =
  print(
    "\n---Calculator---\n" +
      "1. Calculate Expression\n" +
      "2. Calculate a shape's area\n" +
      "3. Exit\n" +
      "Enter: "
  )
  readInt()

def expCalc(): Unit = ???

def shapeArea(): Unit =
  print(
    "\nSelect from following shapes:\n" +
      "1. Triangle\n" +
      "2. Rectangle\n" +
      "3. Regular Pentagon\n" +
      "4. Regular Hexagon\n" +
      "5. Regular Octagon\n" +
      "6. Circle\n" +
      "7. Go Back\n" +
      "Enter: "
  )

  readInt() match
    case 1 => println("Area: " + Triangle().calcArea())
    case 2 => println("Area: " + Rectangle().calcArea())
    case 3 => println("Area: " + Pentagon().calcArea())
    case 4 => println("Area: " + Hexagon().calcArea())
    case 5 => println("Area: " + Octagon().calcArea())
    case 6 => println("Area: " + Circle().calcArea())
    case 7 => return

@main def main: Unit =

  var looping = true
  while looping do
    main_menu match
      case 1 => expCalc()
      case 2 => shapeArea()
      case 3 => looping = false
      case _ => println("Invalid option selected")
