import scala.io.StdIn.readLine
import scala.io.StdIn.readInt
import scala.io.StdIn.readDouble
import scala.math._
import scala.util.boundary, boundary.break

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



def expCalc(): Unit = {

  print("Enter an expression: ");
  val input: String = readLine();

  evalString(input) match{
    case Some(value) => println("Result: " + value);
    case None => println("Invalid expression");
  }
}


def evalString(str: String): Option[String] = {

  var s: String = removeSpaces(str);

  var openingLocation: Int = -1;
  var closingLocation: Int = -1;
  var opening: Int = 0;
  var closing: Int = 0;
  var i: Int = 0;

  //go through each parenthesis and do a recursive call
  while (i < s.length()){
    
    if(s.charAt(i) == '('){
      opening += 1;

      if(openingLocation == -1){
        openingLocation = i;
      }
    }
    else if(s.charAt(i) == ')'){

      closing += 1;
      closingLocation = i;

      //do a recursive call if valid parentheses are found and reset the vars to start looking for other parentheses
      if(opening != 0 && closing != 0 && opening == closing){
        val result: String = evalString(s.substring(openingLocation + 1, closingLocation)).get;
        s = replaceStr(s, openingLocation, closingLocation, result);
        i = 0;
        openingLocation = -1;
        closingLocation = -1;
        opening = 0;
        closing = 0;
      }
    }

    //if the parenthesis is ordered incorrectly, break out. The next if-then will cause return None
    if(closing > opening){
      i = s.length();
    }

    i += 1;
  }

  //if there is not a closing parenthesis for every opening and vice versa
  if(opening != closing){
    return None;
  }

  //get the number left of an operator
  def leftParameter(str: String, index: Int): String = {
    if(index - 2 == -1){
      str.charAt(index - 1) + "";
    }
    else if(isOperatorEN(str.charAt(index - 1)) || str.charAt(index - 1) == '-' && !isOperator(str.charAt(index - 2))){
      "";
    }
    else{
      leftParameter(str, index - 1) + str.charAt(index - 1);
    }
  }

  //get the number right of an operator
  def rightParameter(str: String, index: Int): String = {
    if(index + 2 == str.length()){
      str.charAt(index + 1) + "";
    }
    else if(isOperatorEN(str.charAt(index + 1)) && !isOperator(str.charAt(index))){
      "";
    } else{
      str.charAt(index + 1) + rightParameter(str, index + 1);
    }
  }

  i = 1;

  //first pass simplifying the *, /, and % operators
  while(i < s.length() - 1){
    var c: Char = s.charAt(i);

    if(c == '*'){
      val left: String = leftParameter(s, i);
      val right: String = rightParameter(s, i);
      val result: String = cutScientificNotation((left.toDouble * right.toDouble).toString);
      s = replaceStr(s, i - left.length(), i + right.length(), result);
      i = 0;
    } 
    else if(c == '/'){
      val left: String = leftParameter(s, i);
      val right: String = rightParameter(s, i);
      val result: String = cutScientificNotation((left.toDouble / right.toDouble).toString);
      s = replaceStr(s, i - left.length(), i + right.length(), result);
      i = 0;
    } 
    else if(c == '%'){
      val left: String = leftParameter(s, i);
      val right: String = rightParameter(s, i);
      val result: String = cutScientificNotation((left.toDouble % right.toDouble).toString());
      s = replaceStr(s, i - left.length(), i + right.length(), result);
      i = 0;
    }
    i += 1;
  }

  i = 1;

  //second pass simplifying the + and - operators
  while(i < s.length() - 1){
    var c: Char = s.charAt(i);

    if(c == '+'){
      val left: String = leftParameter(s, i);
      val right: String = rightParameter(s, i);
      val result: String = cutScientificNotation((left.toDouble + right.toDouble).toString);
      s = replaceStr(s, i - left.length(), i + right.length(), result);
      i = 0;
    } 
    else if(c == '-'){
      val left: String = leftParameter(s, i);
      val right: String = rightParameter(s, i);
      val result: String = cutScientificNotation((left.toDouble - right.toDouble).toString);
      s = replaceStr(s, i - left.length(), i + right.length(), result);
      i = 0;
    } 
    i += 1;
  }

  Some(s);

}

//scientific notation numbers cause problems so they are set to zero instead
def cutScientificNotation(str: String): String = {
  if(str.contains("E")){
    "0";
  } else{
    str;
  }
}

def invalidOperatorCheck(str: String): Boolean = {

  def check(str: String, num: Boolean): Boolean = {

    if(str.length() == 0){
      //the last character should be a number, not an operator
      if(num){
        return true;
      } else{
        return false;
      }
    }

    val c: Char = str.charAt(0);
    val currentlyNum: Boolean = !(isOperator(c));

    //if there are two operator characters in a row, return false
    if(!num && !currentlyNum){
      false;
    } else{
      check(str.substring(1, str.length()), currentlyNum);
    }
  }

  val c: Char = str.charAt(0);
  val num: Boolean = !(isOperator(c));
  
  //if the first character is an operator instead of a number then the whole thing is invalid
  if(num){
    check(str, num);
  } else{
    false;
  }



}



def removeSpaces(str: String): String = {

  if(str.length() == 0){
    return ""
  }
  
  if (str.charAt(0) != ' '){
    str.charAt(0) + removeSpaces(str.substring(1, str.length()))
  } 
  else{
    removeSpaces(str.substring(1, str.length()))
  } 
}


def replaceStr(str: String, start: Int, end: Int, replace: String): String = {
  str.substring(0, start) + replace + str.substring(end + 1, str.length());
}


def isOperator(c: Char): Boolean = {
  (c == '+' || c == '-' || c == '*' || c == '/' || c == '%');
}

//checks if c is any operator other than the subtraction operator (excluding negative)
//this is here because of situations with negative numbers where "-" represents negative, not subtraction
def isOperatorEN(c: Char): Boolean = {
  (c == '+' || c == '*' || c == '/' || c == '%');
}


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
  //println(evalString("(5 + 2 * (6%4) + 7) - 2 * (3-8)"))
  var looping = true
  while looping do
    main_menu match
      case 1 => expCalc()
      case 2 => shapeArea()
      case 3 => looping = false
      case _ => println("Invalid option selected")
