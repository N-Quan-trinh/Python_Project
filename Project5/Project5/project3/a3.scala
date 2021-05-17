
package main


class MatchChecker(inp: String){
  var input: String = inp
  var index: Int = 0
}

//S Class
abstract class S{
  def matchCheck(MC: MatchChecker): Boolean
}

//E Class
case class E(l: T, r: Option[E2]) extends S{
  def matchCheck(MC: MatchChecker): Boolean = {
    //read the temporary index
    val temp=MC.index
    //check the left
    if(l.matchCheck(MC)){
      //return true if the left is ok
      true
    }

    else {
      //check the right
      r match {
        //if we have something...
        case Some(r) =>
          //reset the index
          MC.index = temp
          //check the right
          r.matchCheck(MC)
        case None => false
      }
    }
  }
}

//E2 Class
case class E2(l: E) extends S {
  //just read what we have
  def matchCheck(MC: MatchChecker): Boolean = l.matchCheck(MC)
}

//T Case Class
case class T(l: F, r: Option[T2]) extends S{
  def matchCheck(MC: MatchChecker): Boolean = {
    //check the left side
    if(!l.matchCheck(MC)) {

      false

    } else
      r match {

        case Some(r) =>
          //check the right
          r.matchCheck(MC)

        case None => true
      }
  }
}

//T2 Case Class
case class T2(l: F, r: Option[T2]) extends S {
  def matchCheck(MC: MatchChecker): Boolean = {
    //check the left side
    if(!l.matchCheck(MC)) {
      //if the left side isn't right, return false
      false

    } else
      r match {

        case Some(r) =>
          //check the right
          r.matchCheck(MC)
        //otherwise, we can assume that the left is true and it's the only thing that exists.
        case None => true
      }
  }
}

case class F(l: A, r: Option[F2]) extends S{
  def matchCheck(MC: MatchChecker): Boolean = {
    //set the temp index
    val temp=MC.index
    //check the left side
    if(l.matchCheck(MC)){
      true
    }
    //If it's false...
    else
      r match {
        //We have a ?
        case Some(r) =>
          //reset the index
          MC.index=temp
          //check the right (which ultimately returns true)
          r.matchCheck(MC)
        case None => false
      }
  }
}

case class F2(l: Option[F2]) extends S {
  //pass up true if we get to this point
  def matchCheck(MC: MatchChecker): Boolean = true
}

//abstract because it could return either a terminal or a non-terminal
abstract class A extends S{
}

case class A2(l: E) extends A{
  //just read what we have
  def matchCheck(MC: MatchChecker): Boolean = l.matchCheck(MC)
}

case class C(v: Char) extends A{
  def matchCheck(MC: MatchChecker): Boolean = {

    var a1: Boolean = false

    if(MC.index < MC.input.length){

      if(v=='.' || MC.input(MC.index)==v) {
        MC.index+=1
        //only set as true if the conditions are met
        a1 = true
      }
    }
    //return the result
    a1
  }
}

//RecursiveDescent with an input pattern string parameter
class RecursiveDescent(input:String) {

  //index in the expression
  var index = 0

  //parse the input
  def parseS(): S = parseE()

  def parseE(): E = {
    E(parseT(), parseE2())
  }

  def parseE2(): Option[E2] = {
    if (index < input.length && input(index) == '|') {
      // Advance past the '|'
      index += 1;
      //return a parse E
      Some(E2(parseE()))
    }
    else None
  }

  def parseT(): T = T(parseF(), parseT2())

  def parseT2(): Option[T2] = {

    if (index < input.length &&
      (input(index) == ' ' || input(index) == '.' ||
        input(index).isLetterOrDigit || input(index) == '(')) {
      //parse accordingly
      Some(T2(parseF(), parseT2()))
    }
    else None
  }

  def parseF(): F = F(parseA(), parseF2())


  def parseF2(): Option[F2] = {
    if (index < input.length && input(index) == '?') {
      // Advance past '?'
      index += 1;
      Some(F2(parseF2()))
    }
    else None
  }

  def parseA(): A = {
    //if we see an open parentheses
    if(input(index) == '('){
      //bump past the parentheses
      index+=1
      //parse accordingly
      parseA2()
    }
    else{
      //return a constant
      val a1 = C(input(index))
      //but bump the index accordingly first
      index+=1
      a1
    }
  }

  //returns an E
  def parseA2(): A2 = {
    val a1 = A2(parseE())
    if(input(index) == ')') {
      //simply bump past it
      index += 1
    }
    a1
  }
}


object Main {
  def main(args: Array[String]) {
    //pattern prompt
    println("Please enter a pattern")
    //pattern input
    val patternString = scala.io.StdIn.readLine()
    //define a new recursive descent
    val patternRD = new RecursiveDescent(patternString)
    //begin by parsing E
    val patternTree: S = patternRD.parseE()

    print("Parse Tree")
    print(patternTree)

    //choice variable for user interaction
    var choice: Int = 0
    //while loop until our choice is altered
    while(choice != 1){
      println("Please enter an input string to be checked")
      //read the input string
      val input = scala.io.StdIn.readLine()
      //set a match checker
      val MC: MatchChecker = new MatchChecker(input)
      //define the output, given the result of the parse tree
      val output=patternTree.matchCheck(MC)
      //define a display to convert the boolean to a string result
      var display: String = null
      //if the output is a match and we are within the specified input length (no garbage characters afterwards)
      if(output && MC.index==input.length)
        display = "Match!"
      else
        display = "No Match!"
      println(display)
      println("Would you like to enter another string to test? 0=yes, 1=no")
      choice = scala.io.StdIn.readInt()
    }
  }
}