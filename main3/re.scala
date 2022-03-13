// Main Part 3 about Regular Expression Matching
//=============================================

object M3 {

// Regular Expressions
abstract class Rexp
case object ZERO extends Rexp
case object ONE extends Rexp
case class CHAR(c: Char) extends Rexp
case class ALTs(rs: List[Rexp]) extends Rexp      // alternatives
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp   // sequence
case class STAR(r: Rexp) extends Rexp             // star


// some convenience for typing regular expressions

//the usual binary choice can be defined in terms of ALTs
def alt(r1: Rexp, r2: Rexp) = ALTs(List(r1, r2))


import scala.language.implicitConversions    
import scala.language.reflectiveCalls 

def charlist2rexp(s: List[Char]): Rexp = s match {
  case Nil => ONE
  case c::Nil => CHAR(c)
  case c::s => SEQ(CHAR(c), charlist2rexp(s))
}

implicit def string2rexp(s: String): Rexp = charlist2rexp(s.toList)

implicit def RexpOps (r: Rexp) = new {
  def | (s: Rexp) = ALT(r, s)
  def % = STAR(r)
  def ~ (s: Rexp) = SEQ(r, s)
}

implicit def stringOps (s: String) = new {
  def | (r: Rexp) = ALT(s, r)
  def | (r: String) = ALT(s, r)
  def % = STAR(s)
  def ~ (r: Rexp) = SEQ(s, r)
  def ~ (r: String) = SEQ(s, r)
}

// (1) Complete the function nullable according to
// the definition given in the coursework; this 
// function checks whether a regular expression
// can match the empty string and Returns a boolean
// accordingly.

def nullable (r: Rexp) : Boolean = r match {
  case ZERO => false
  case ONE => true
  case CHAR(_) => false
  case ALTs(rs) => rs.exists(nullable)
  case SEQ(r1, r2) => nullable(r1) && nullable(r2)
  case STAR(_) => true
}


// (2) Complete the function der according to
// the definition given in the coursework; this
// function calculates the derivative of a 
// regular expression w.r.t. a character.

def der (c: Char, r: Rexp) : Rexp = r match {
  case ZERO => ZERO
  case ONE => ZERO
  case CHAR(d) => if (c == d) ONE else ZERO
  case ALTs(rs) => ALTs(rs.map(der(c, _)))
  case SEQ(r1, r2) => if (nullable(r1)) ALT(SEQ(der(c, r1), r2), der(c, r2))
  else SEQ(der(c, r1), r2)
  case STAR(r) => SEQ(der(c, r), STAR(r))
}


// (3) Implement the flatten function flts. It
// deletes 0s from a list of regular expressions
// and also 'spills out', or flattens, nested 
// ALTernativeS.

def flts(rs: List[Rexp]) : List[Rexp] = rs match {
  case Nil => Nil
  case ZERO::rs => flts(rs)
  case r::rs => r match {
    case ALTs(rs) => flts(rs) ::: flts(rs)
    case _ => r::flts(rs)
  }
}



// (4) Complete the simp function according to
// the specification given in the coursework description; 
// this function simplifies a regular expression from
// the inside out, like you would simplify arithmetic 
// expressions; however it does not simplify inside 
// STAR-regular expressions. Use the _.distinct and 
// flts functions.

def simp(r: Rexp) : Rexp = r match {
  case SEQ(r1, r2) => {
    SEQ(simp(r1), simp(r2)) match {
      case SEQ(r1, ZERO) => ZERO
      case SEQ(ZERO, r2) => ZERO
      case SEQ(r1, ONE) => r1
      case SEQ(ONE, r2) => r2
      case _ => SEQ(r1, r2)
    }
  }

  case alt(r1, r2) => {
    alt(simp(r1), simp(r2)) match {
      case alt(ZERO, r2) => ZERO
      case alt(r1, ZERO) => ZERO
      case alt(ONE, r2) => r2
      case alt(r1, ONE) => r1
      case _ => alt(r1, r2)
    }
  }

  case _ => r
}


// (5) Complete the two functions below; the first 
// calculates the derivative w.r.t. a string; the second
// is the regular expression matcher taking a regular
// expression and a string and checks whether the
// string matches the regular expression

def ders (s: List[Char], r: Rexp) : Rexp = s match {
  case Nil => r
  case c::i => ders(i, simp(der(c, r)))
}

def matcher(r: Rexp, s: String): Boolean = {
  val d = ders(s.toList, r)
  nullable(d)
}


// (6) Complete the size function for regular
// expressions according to the specification 
// given in the coursework.

def size(r: Rexp): Int = r match {
  case ZERO => 0
  case ONE => 1
  case CHAR(_) => 1
  case ALT(r1, r2) => size(r1) + size(r2)
  case SEQ(r1, r2) => size(r1) + size(r2)
  case STAR(r) => size(r) + 1
}


// some testing data

/*
matcher(("a" ~ "b") ~ "c", "abc")  // => true
matcher(("a" ~ "b") ~ "c", "ab")   // => false

// the supposedly 'evil' regular expression (a*)* b
val EVIL = SEQ(STAR(STAR(CHAR('a'))), CHAR('b'))

matcher(EVIL, "a" * 1000 ++ "b")   // => true
matcher(EVIL, "a" * 1000)          // => false

// size without simplifications
size(der('a', der('a', EVIL)))             // => 28
size(der('a', der('a', der('a', EVIL))))   // => 58

// size with simplification
size(simp(der('a', der('a', EVIL))))           // => 8
size(simp(der('a', der('a', der('a', EVIL))))) // => 8

// Python needs around 30 seconds for matching 28 a's with EVIL. 
// Java 9 and later increase this to an "astonishing" 40000 a's in
// 30 seconds.
//
// Lets see how long it really takes to match strings with 
// 5 Million a's...it should be in the range of a couple
// of seconds.

def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  "%.5f".format((end - start)/(i * 1.0e9))
}

for (i <- 0 to 5000000 by 500000) {
  println(s"$i ${time_needed(2, matcher(EVIL, "a" * i))} secs.") 
}

// another "power" test case 
simp(Iterator.iterate(ONE:Rexp)(r => SEQ(r, ONE | ONE)).drop(50).next()) == ONE

// the Iterator produces the rexp
//
//      SEQ(SEQ(SEQ(..., ONE | ONE) , ONE | ONE), ONE | ONE)
//
//    where SEQ is nested 50 times.

*/

}
