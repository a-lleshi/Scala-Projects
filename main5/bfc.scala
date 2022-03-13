// Main Part 5 about a "Compiler" for the Brainf*** language
//============================================================


object M5b {


// !!! Copy any function you need from file bf.scala !!!
//
// If you need any auxiliary function, feel free to 
// implement it, but do not make any changes to the
// templates below.


// DEBUGGING INFORMATION FOR COMPILERS!!!
//
// Compiler, even real ones, are fiendishly difficult to get
// to produce correct code. One way to debug them is to run
// example programs ``unoptimised''; and then optimised. Does
// the optimised version still produce the same result?


// for timing purposes
def time_needed[T](n: Int, code: => T) = {
  val start = System.nanoTime()
  for (i <- 0 until n) code
  val end = System.nanoTime()
  (end - start)/(n * 1.0e9)
}


type Mem = Map[Int, Int]

import io.Source
import scala.util._

def load_bff(name: String) : String = {
  try {
    val source = Source.fromFile(name)
    val lines = source.mkString
    source.close
    lines
  } catch {
    case _ : Throwable => ""
  }
}

def sread(mem: Mem, mp: Int) : Int = {
  mem.getOrElse(mp, 0)
}

def swrite(mem: Mem, mp: Int, v: Int) : Mem = {
  mem + (mp -> v)
}

def write(mem: Mem, mp: Int, v: Int) : Mem = {
  mem + (mp -> v)
}

def jumpRight(prog: String, pc: Int, level: Int) : Int = {
  if (pc > prog.length-2 || prog(pc) == ']' && level == 0) pc + 1
  else if (prog(pc) == ']' && level != 0) jumpRight(prog, pc + 1, level - 1)
  else if (prog(pc) == '[') jumpRight(prog, pc + 1, level + 1)
  else jumpRight(prog, pc + 1, level)
}

def jumpLeft(prog: String, pc: Int, level: Int) : Int = {
  if (pc < 0) pc
  else if (prog(pc) == '[' && level == 0) pc + 1
  else if (prog(pc) == '[' && level != 0) jumpLeft(prog, pc - 1, level - 1)
  else if (prog(pc) == ']') jumpLeft(prog, pc - 1, level + 1)
  else jumpLeft(prog, pc - 1, level)
}

def compute(prog: String, pc: Int, mp: Int, mem: Mem) : Mem = {
  if (pc < 0 || pc >= prog.length) mem
  else {
    prog(pc) match {
      case '>' => compute(prog, pc + 1, mp + 1, mem)
      case '<' => compute(prog, pc + 1, mp - 1, mem)
      case '+' => compute(prog, pc + 1, mp, write(mem, mp, sread(mem, mp) + 1))
      case '-' => compute(prog, pc + 1, mp, write(mem, mp, sread(mem, mp) - 1))
      case '.' => compute(prog, pc + 1, mp, mem)
      case ',' => compute(prog, pc + 1, mp, write(mem, mp, scala.io.StdIn.readInt()))
      case '[' => {
        if (sread(mem, mp) == 0) compute(prog, jumpRight(prog, pc + 1, 0), mp, mem)
        else compute(prog, pc+1, mp, mem)
      }
      case ']' => {
        if (sread(mem, mp) != 0) compute(prog, jumpLeft(prog, pc - 1, 0), mp, mem)
        else compute(prog, pc+1, mp, mem)
      }
      case _ => compute(prog, pc + 1, mp, mem)
    }
  }
}

def run(prog: String, m: Mem = Map()) = {
  compute(prog, 0, 0, m)
}

// TASKS
//=======

// (5) Write a function jtable that precomputes the "jump
//     table" for a bf-program. This function takes a bf-program 
//     as an argument and Returns a Map[Int, Int]. The 
//     purpose of this map is to record the information about
//     pc positions where '[' or a ']' are stored. The information
//     is to which pc-position do we need to jump next?
// 
//     For example for the program
//    
//       "+++++[->++++++++++<]>--<+++[->>++++++++++<<]>>++<<----------[+>.>.<+<]"
//
//     we obtain the map
//
//       Map(69 -> 61, 5 -> 20, 60 -> 70, 27 -> 44, 43 -> 28, 19 -> 6)
//  
//     This states that for the '[' on position 5, we need to
//     jump to position 20, which is just after the corresponding ']'.
//     Similarly, for the ']' on position 19, we need to jump to
//     position 6, which is just after the '[' on position 5, and so
//     on. The idea is to not calculate this information each time
//     we hit a bracket, but just look up this information in the 
//     jtable. You can use the jumpLeft and jumpRight functions
//     from Part 1 for calculating the jtable.
//
//     Then adapt the compute and run functions from Part 1 
//     in order to take advantage of the information stored in the jtable. 
//     This means whenever jumpLeft and jumpRight was called previously,
//     you should immediately look up the jump address in the jtable.
 

def jtable(pg: String) : Map[Int, Int] = {
  (0 until pg.length).foldLeft(Map[Int, Int]())((acc, pc) => {
    if (pg(pc) == '[') acc + (pc -> jumpRight(pg, pc+1, 0))
    else if (pg(pc) == ']') acc + (pc -> jumpLeft(pg, pc-1, 0))
    else acc
  })
}


// testcase
//
// jtable("""+++++[->++++++++++<]>--<+++[->>++++++++++<<]>>++<<----------[+>.>.<+<]""")
// =>  Map(69 -> 61, 5 -> 20, 60 -> 70, 27 -> 44, 43 -> 28, 19 -> 6)


def compute2(pg: String, tb: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem = {
  if (pc < 0 || pc >= pg.length) mem
  else {
    pg(pc) match {
      case '>' => compute2(pg, tb, pc + 1, mp + 1, mem)
      case '<' => compute2(pg, tb, pc + 1, mp - 1, mem)
      case '+' => compute2(pg, tb, pc + 1, mp, write(mem, mp, sread(mem, mp) + 1))
      case '-' => compute2(pg, tb, pc + 1, mp, write(mem, mp, sread(mem, mp) - 1))
      case '.' => compute2(pg, tb, pc + 1, mp, mem)
      case ',' => compute2(pg, tb, pc + 1, mp, write(mem, mp, scala.io.StdIn.readInt()))
      case '[' => {
        if (sread(mem, mp) == 0) compute2(pg, tb, tb(pc), mp, mem)
        else compute2(pg, tb, pc+1, mp, mem)
      }
      case ']' => {
        if (sread(mem, mp) != 0) compute2(pg, tb, tb(pc), mp, mem)
        else compute2(pg, tb, pc+1, mp, mem)
      }
      case _ => compute2(pg, tb, pc + 1, mp, mem)
    }
  }
}
def run2(pg: String, m: Mem = Map()) = {
  compute2(pg, jtable(pg), 0, 0, m)
}


// testcases
// time_needed(1, run2(load_bff("benchmark.bf")))
// time_needed(1, run2(load_bff("sierpinski.bf")))



// (6) Write a function optimise which deletes "dead code" (everything
// that is not a bf-command) and also replaces substrings of the form
// [-] by a new command 0. The idea is that the loop [-] just resets the
// memory at the current location to 0. In the compute3 and run3 functions
// below you implement this command by writing the number 0 to mem(mp), 
// that is write(mem, mp, 0). 
//
// The easiest way to modify a string in this way is to use the regular
// expression """[^<>+-,\[\]]""", which recognises everything that is 
// not a bf-command and replace it by the empty string. Similarly the
// regular expression """\[-\]""" finds all occurrences of [-] and 
// by using the Scala method .replaceAll you can replace it with the 
// string "0" standing for the new bf-command.

def optimise(s: String) : String = {
  s.replaceAll("[^<>+-.\\[\\]]", "").replaceAll("\\[-\\]", "0")
}

def compute3(pg: String, tb: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem = {
  if (pc < 0 || pc >= pg.length) mem
  else {
    pg(pc) match {
      case '>' => compute3(pg, tb, pc + 1, mp + 1, mem)
      case '<' => compute3(pg, tb, pc + 1, mp - 1, mem)
      case '+' => compute3(pg, tb, pc + 1, mp, write(mem, mp, sread(mem, mp) + 1))
      case '-' => compute3(pg, tb, pc + 1, mp, write(mem, mp, sread(mem, mp) - 1))
      case '.' => compute3(pg, tb, pc + 1, mp, mem)
      case ',' => compute3(pg, tb, pc + 1, mp, write(mem, mp, scala.io.StdIn.readInt()))
      case '[' => {
        if (sread(mem, mp) == 0) compute3(pg, tb, tb(pc), mp, mem)
        else compute3(pg, tb, pc+1, mp, mem)
      }
      case ']' => {
        if (sread(mem, mp) != 0) compute3(pg, tb, tb(pc), mp, mem)
        else compute3(pg, tb, pc+1, mp, mem)
      }
      case '0' => compute3(pg, tb, pc + 1, mp, write(mem, mp, 0))
      case _ => compute3(pg, tb, pc + 1, mp, mem)
    }
  }
}

def run3(pg: String, m: Mem = Map()) = {
  compute3(optimise(pg), jtable(optimise(pg)), 0, 0, m)
}


// testcases
//
// optimise(load_bff("benchmark.bf"))          // should have inserted 0's
// optimise(load_bff("mandelbrot.bf")).length  // => 11205
// 
// time_needed(1, run3(load_bff("benchmark.bf")))



// (7)  Write a function combine which replaces sequences
// of repeated increment and decrement commands by appropriate
// two-character commands. For example for sequences of +
//
//              orig bf-cmds  | replacement
//            ------------------------------
//              +             | +A 
//              ++            | +B
//              +++           | +C
//                            |
//              ...           |
//                            | 
//              +++....+++    | +Z
//                (where length = 26)
//
//  Similar for the bf-command -, > and <. All other commands should
//  be unaffected by this change.
//
//  Adapt the compute4 and run4 functions such that they can deal
//  appropriately with such two-character commands.


def combine(s: String) : String = {
  val s2 = s.replaceAll("(.)\\1+", "+A")
  val s3 = s2.replaceAll("(.)\\1+", "+B")
  val s4 = s3.replaceAll("(.)\\1+", "+C")
  val s5 = s4.replaceAll("(.)\\1+", "+D")
  val s6 = s5.replaceAll("(.)\\1+", "+E")
  val s7 = s6.replaceAll("(.)\\1+", "+F")
  val s8 = s7.replaceAll("(.)\\1+", "+G")
  val s9 = s8.replaceAll("(.)\\1+", "+H")
  val s10 = s9.replaceAll("(.)\\1+", "+I")
  val s11 = s10.replaceAll("(.)\\1+", "+J")
  val s12 = s11.replaceAll("(.)\\1+", "+K")
  val s13 = s12.replaceAll("(.)\\1+", "+L")
  val s14 = s13.replaceAll("(.)\\1+", "+M")
  val s15 = s14.replaceAll("(.)\\1+", "+N")
  val s16 = s15.replaceAll("(.)\\1+", "+O")
  val s17 = s16.replaceAll("(.)\\1+", "+P")
  val s18 = s17.replaceAll("(.)\\1+", "+Q")
  val s19 = s18.replaceAll("(.)\\1+", "+R")
  val s20 = s19.replaceAll("(.)\\1+", "+S")
  val s21 = s20.replaceAll("(.)\\1+", "+T")
  val s22 = s21.replaceAll("(.)\\1+", "+U")
  val s23 = s22.replaceAll("(.)\\1+", "+V")
  val s24 = s23.replaceAll("(.)\\1+", "+W")
  val s25 = s24.replaceAll("(.)\\1+", "+X")
  val s26 = s25.replaceAll("(.)\\1+", "+Y")
  val s27 = s26.replaceAll("(.)\\1+", "+Z")  
  s2 + s3 + s4 + s5 + s6 + s7 + s8 + s9 + s10 + s11 + s12 + s13 + s14 + s15 + s16 + s17 + s18 + s19 + s20 + s21 + s22 + s23 + s24 + s25 + s26 + s27
}
// testcase
// combine(load_bff("benchmark.bf"))


def compute4(pg: String, tb: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem = {
  if (pc < 0 || pc >= pg.length) mem
  else {
    pg(pc) match {
      case '>' => compute4(pg, tb, pc + 1, mp + 1, mem)
      case '<' => compute4(pg, tb, pc + 1, mp - 1, mem)
      case '+' => compute4(pg, tb, pc + 1, mp, write(mem, mp, sread(mem, mp) + 1))
      case '-' => compute4(pg, tb, pc + 1, mp, write(mem, mp, sread(mem, mp) - 1))
      case '.' => compute4(pg, tb, pc + 1, mp, mem)
      case ',' => compute4(pg, tb, pc + 1, mp, write(mem, mp, scala.io.StdIn.readInt()))
      case '[' => {
        if (sread(mem, mp) == 0) compute4(pg, tb, tb(pc), mp, mem)
        else compute4(pg, tb, pc+1, mp, mem)
      }
      case ']' => {
        if (sread(mem, mp) != 0) compute4(pg, tb, tb(pc), mp, mem)
        else compute4(pg, tb, pc+1, mp, mem)
      }
      case '0' => compute4(pg, tb, pc + 1, mp, write(mem, mp, 0))
      case _ => compute4(pg, tb, pc + 1, mp, mem)
    }
  }
}


// should call first optimise and then combine on the input string
//
def run4(pg: String, m: Mem = Map()) = {
  compute4(optimise(pg), jtable(optimise(pg)), 0, 0, m)
}


// testcases
// combine(optimise(load_bff("benchmark.bf"))) // => """>A+B[<A+M>A-A]<A[[....."""

// testcases (they should now run much faster)
// time_needed(1, run4(load_bff("benchmark.bf")))
// time_needed(1, run4(load_bff("sierpinski.bf"))) 
// time_needed(1, run4(load_bff("mandelbrot.bf")))


}
