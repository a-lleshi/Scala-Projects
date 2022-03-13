// Shunting Yard Algorithm 
// including Associativity for Operators 
// =====================================

object C3b {


// type of tokens
type Toks = List[String]

// helper function for splitting strings into tokens
def split(s: String) : Toks = s.split(" ").toList

// left- and right-associativity
abstract class Assoc
case object LA extends Assoc
case object RA extends Assoc


// power is right-associative,
// everything else is left-associative
def assoc(s: String) : Assoc = s match {
  case "^" => RA
  case _ => LA
}


// the precedences of the operators
val precs = Map("+" -> 1,
  		"-" -> 1,
		"*" -> 2,
		"/" -> 2,
                "^" -> 4)

// the operations in the basic version of the algorithm
val ops = List("+", "-", "*", "/", "^")

// (3) Implement the extended version of the shunting yard algorithm.
// This version should properly account for the fact that the power 
// operation is right-associative. Apart from the extension to include
// the power operation, you can make the same assumptions as in 
// basic version.

def is_num(s: String) : Boolean = s.matches("[0-9]+")

def is_op(op: String) : Boolean = ops.contains(op)

def prec(op1: String, op2: String) : Boolean = {
  if (op1 == "^") precs(op1) > precs(op2)
  else precs(op1) >= precs(op2)
}

def syard(toks: Toks, st: Toks = Nil, out: Toks = Nil) : Toks = {
  toks match {
    case tok :: rest if tok == "(" => syard(rest, tok :: st, out)
    case tok :: rest if tok == ")" => { val index = st.indexOf("(")
      syard(rest, st.drop(index+1), out:::st.take(index))
    }
    case tok :: rest if is_op(tok) => {
      if (st.isEmpty || st.head == "(") syard(rest, tok :: st, out)
      else if (prec(st.head, tok)) syard(rest, tok :: st.drop(1), out :+ st.head)
      else syard(rest, tok :: st, out)
    }
    case tok :: rest if is_num(tok) => {
      syard(rest, st, out:::tok::Nil)
    }
    case _ => out ::: st
  }
}

// test cases
// syard(split("3 + 4 * 8 / ( 5 - 1 ) ^ 2 ^ 3"))  // 3 4 8 * 5 1 - 2 3 ^ ^ / +


// (4) Implement a compute function that produces an Int for an
// input list of tokens in postfix notation.

def calculatePower(x: Int, y: Int) : Int = {
  Math.pow(x, y).toInt
}

def compute(toks: Toks, st: List[Int] = Nil) : Int = {
  	toks match {
		case tok :: rest if is_num(tok) => compute(rest, tok.toInt :: st)
		case tok :: rest if is_op(tok) => {
			val op1 = st.head
			val op2 = st(1)
			compute(rest, ((tok match {
				case "+" => op1 + op2
				case "-" => op2 - op1
				case "*" => op1 * op2
				case "/" => op2 / op1
        case "^" => calculatePower(op2, op1)
			}) :: st.drop(2)))
		}
		case _ => st.head
	}
}


// test cases
// compute(syard(split("3 + 4 * ( 2 - 1 )")))   // 7
// compute(syard(split("10 + 12 * 33")))       // 406
// compute(syard(split("( 5 + 7 ) * 2")))      // 24
// compute(syard(split("5 + 7 / 2")))          // 8
// compute(syard(split("5 * 7 / 2")))          // 17
// compute(syard(split("9 + 24 / ( 7 - 3 )"))) // 15
// compute(syard(split("4 ^ 3 ^ 2")))      // 262144
// compute(syard(split("4 ^ ( 3 ^ 2 )")))  // 262144
// compute(syard(split("( 4 ^ 3 ) ^ 2")))  // 4096
// compute(syard(split("( 3 + 1 ) ^ 2 ^ 3")))   // 65536

}
