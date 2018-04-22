import sexp._

sealed abstract class Exp
case class Literal(v: Int) extends Exp
case class Add(lhs: Exp, rhs: Exp) extends Exp
case class Multiply(lhs: Exp, rhs: Exp) extends Exp
case class Let(v : String, rhs : Exp, body : Exp) extends Exp
case class Ref(v : String) extends Exp

case class Call(lhs : String, rhs : Exp) extends Exp
case class Def(name : String, argument : String, body : Exp)
case class Program(defs : List[Def], exp : Exp)
"""
((define (square n)
		(* n n))
	(square 5))
""" 
Program(List(Def("square", "n", Multiply(Ref("n", ref("n")))


def parseProgram(p : SExp) : Program = {
	//case SList(lhs, rhs) => Call(lhs, parseArith(rhs))
	//case SList(SSymbol("define"), SList(SList(SSymbol(name), SSymbol(arg)), body) => Def(name, arg, parseArith(body)) // Not sure here
	case SList(l) => Program(parseArith(aDef), parseArith(exp)) // Not sure here
}

def parseArith(e: SExp) : Exp = {
    e match {
      case SInt(v) => Literal(v)
      case SList(SSymbol("+"), l, r) => Add(parseArith(l), parseArith(r))
      case SList(SSymbol("*"), l, r) => Multiply(parseArith(l), parseArith(r))
			case SSymbol(id) => Ref(id)
			case SList(SSymbol("let"), SList(SList(SSymbol(id), r)), body) => Let(id, parseArith(r), parseArith(body))
			case SList(SSymbol(id), e) =>  // Not sure here
      case _ => throw new IllegalArgumentException("not a valid arithmetic language expression")
    }
}

val ex = parseArith(parseSExp("(* (+ 5 2) 1)"))

def eval(e : String) : Int = interp(parseArith(parseSExp(e)), Map())

type Env = Map[String, Int]

def interp(e: Exp, env : Env) : Int = {
    e match {
        case Literal(v) => v
        case Add(l,r) => {
          val lv = interp(l, env)
          val rv = interp(r, env)
          lv + rv
        }
        case Multiply(l,r) => {
          val lv = interp(l, env)
          val rv = interp(r, env)
          lv * rv
        }
        case Ref(id) => env.get(id) match {
						case None => throw new RuntimeException("unbound variable")
						case Some(v) => v
				}
				case Let(id, rhs, body) => {
					val rhsVal = interp(rhs, env) 
					val newEnv = env + (id -> rhsVal)
					interp(body, newEnv)
				}    
    }
}
