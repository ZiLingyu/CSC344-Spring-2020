import scala.util.matching.Regex

//  S -: E$
abstract class S {
  def eval(env: Main.Environment): Int
}

// E -: T E2
case class E(left: T, right: Option[E2]) extends S {
  def eval(env: Main.Environment): Int = {
    right match {
      case Some(r) => left.eval(env) + r.eval(env)
      case None => left.eval(env)
    }
  }
}

// E2 -: +E
// E2 -: NIL
case class E2(l: E) extends S {
  def eval(env: Main.Environment): Int = l.eval(env)
}

// T -: Terminal T2
case class T(l: Terminal, right: Option[T2]) extends S {
  def eval(env: Main.Environment): Int = {
    val a1: Int = l match {
      case v:Var => v.eval(env)
      case c:Const => c.eval(env)
    }
    right match {
      case Some(r) => a1 * r.eval(env)
      case None => a1
    }
  }
}

// T2 -: *T
// T2 -: NIL
case class T2(l: T) extends S {
  def eval(env: Main.Environment): Int = l.eval(env)
}

abstract class Terminal extends S

// Terminal -: Val
case class Var(n: String) extends Terminal {
  def eval(env: Main.Environment): Int = env(n)
}

// Terminal -: Const
case class Const(v: Int) extends Terminal {
  def eval(env: Main.Environment): Int = v
}

class RecursiveDescent(input:String) {
  val constregex: Regex = "^[0-9]+".r
  val varregex: Regex = "^[A-Za-z]+".r

  var index = 0

  def parseS(): S = parseE()

  def parseE(): E = E(parseT(), parseE2())

  def parseT(): T = T(parseTerminal(), parseT2())

  def parseE2(): Option[E2] = {
    if (index < input.length && input(index) == '+'){
      index+=1; // Advance past +
      Some(E2(parseE()))
    }
    else None
  }

  def parseT2(): Option[T2] = {
    if (index < input.length && input(index) == '*'){
      index+=1; // Advance past +
      Some(T2(parseT()))
    }
    else None
  }

  def parseTerminal(): Terminal = {
    // Get the unparsed part of the string.
    val currStr = input.substring(index)

    // Get either the const or var which is there.
    val consts = constregex.findAllIn(currStr)
    if (consts.hasNext){
      val const: String = consts.next()
      index += const.length()
      Const(const.toInt)
    }
    else {
      val vars = varregex.findAllIn(currStr)
      val varname = vars.next()
      index += varname.length()
      Var(varname)
    }
  }
}

object Main {
  type Environment = String => Int

  def main(args: Array[String]){
    val env: Environment = { case "x" => 2 case "y" => 7 }
    // 2 + 2 * 7 + 7 = 23
    val rd = new RecursiveDescent("x+x*7+y")
    val exp2rd:S = rd.parseE()
    println(exp2rd)
    println(exp2rd.eval(env))
  }
}
