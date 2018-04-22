import sexp._

sealed abstract class Exp
case class Literal(v: SExp) extends Exp
case class Add(lhs: Exp, rhs: Exp) extends Exp
case class Multiply(lhs: Exp, rhs: Exp) extends Exp
//Make function declarations and calls handle multiple arguments.
//Call changed to List[String] and List[Exp] but no changes to parseExp yet
//Part 2 Call:
//case class Call(lhs : String, rhs : List[Exp]) extends Exp 
case class Call(lhs : String, rhs : Exp) extends Exp 
case class Let(v : String, rhs: Exp, body : Exp) extends Exp
case class Ref(v : String) extends Exp
case class If(tf: Exp, truE: Exp, falE: Exp) extends Exp
case class Equal(lhs: Exp, rhs: Exp) extends Exp

//changed from String to List[String] but parseDefine has not been changed yet
//part 2 Def
//case class Def(name : String, argument : List[String], body : Exp)
case class Def(name : String, argument : String, body : Exp)
case class Program(defs : List[Def], exp : Exp)

def parseProgram(p : SExp) : Program = {
  def parseProgHelp(p2 : SExp, acc : List[Def]) : Program = { 
    p2 match {
      case SList(exp) => Program(acc.reverse, parseExp(exp))
      case SCons(first, rest) => parseProgHelp(rest, parseDefine(first) :: acc) 
    }
  }
  parseProgHelp(p, Nil)
}


def parseDefine(p : SExp) : Def = {
  p match {
    case SList(SSymbol("define"), SList(SSymbol(name), SSymbol(arg)), body) => Def(name, arg, parseExp(body))  
  }
}

def parseExp(e: SExp) : Exp = { 
    e match {
      case SInt(v) => Literal(SInt(v))
      case STrue() => Literal(STrue())   
      case SFalse() => Literal(SFalse())
      case SList(SSymbol("+"), l, r) => Add(parseExp(l), parseExp(r))
      case SList(SSymbol("*"), l, r) => Multiply(parseExp(l), parseExp(r))
      case SSymbol(id) => Ref(id)
      case SList(SSymbol("let"), SList(SList(SSymbol(id), rhs)), body) => Let(id, parseExp(rhs), parseExp(body))
      case SList(SSymbol(id), e) => Call(id, parseExp(e))
      case SList(SSymbol("if"), torf, te, fe) => If(parseExp(torf), parseExp(te), parseExp(fe))
      case SList(SSymbol("equal?"), l, r) => Equal(parseExp(l), parseExp(r))
      case _ => throw new IllegalArgumentException("not a valid expression")
    }
}

type Env = Map[String, SExp]  // changed from Int to SExp as the value 
type DefEnv = Map[String, Def] 

def interpExp(e : Exp, env : Env, defEnv : DefEnv) : SExp = e match {
  case Literal(v) => {
		v match {
			case SInt(v2) => SInt(v2)
			case STrue() => STrue() 
			case SFalse() => SFalse()
		}
	}
  case Add(e1, e2) => {
    val v1 = interpExp(e1, env, defEnv)
    val v2 = interpExp(e2, env, defEnv)
    (v1, v2) match {
      case (SInt(n1), SInt(n2)) => SInt(n1 + n2)
      case _ => throw new RuntimeException("can only add integers")
    }
  }
  case Multiply(e1, e2) => {
    val v1 = interpExp(e1, env, defEnv)
    val v2 = interpExp(e2, env, defEnv)
    (v1, v2) match {
      case (SInt(n1), SInt(n2)) => SInt(n1 * n2)
      case _ => throw new RuntimeException("can only multiply integers")
    }
  }
  case Ref(id) => env.get(id) match {
    case None => throw new RuntimeException("unbound variable")
    case Some(v) => v
    }
  case Let(id, rhs, body) =>{
    val rhsVal = interpExp(rhs, env, defEnv)  // problem because the return is no longer an Int
    val newEnv = env + (id -> rhsVal) 
    interpExp(body, newEnv, defEnv)
    }
  case Call(name, exp) => defEnv.get(name) match {
    case None => throw new RuntimeException("no such function")
    case Some(Def(fName, args, bodyDef)) =>                       
      val valExp = interpExp(exp, env, defEnv)                   
      val newEnv = env + (args -> valExp)   // Def(fName, args, exp) ***Problem because return is no longer an Int
      interpExp(bodyDef, newEnv, defEnv)                     
    } 
  case If(torf, tex, fex) => {
    val tf = interpExp(torf, env, defEnv)
    tf match {
      case STrue() => interpExp(tex, env, defEnv)
      case SFalse() => interpExp(fex, env, defEnv)
    }
  }
  case Equal(l, r) => {
    val e1 = interpExp(l, env, defEnv)
    val e2 = interpExp(r, env, defEnv)
    if(e1 == e2){
      STrue()
    }
    else{
      SFalse()
    }
  }
}

def interpProgram(p : Program) : SExp = {  
  val Program(defs, exp) = p          
  val env =  interpProgHelp(defs, Map())  
  interpExp(exp, Map(), env)           
}

def interpProgHelp(l : List[Def], acc : Map[String, Def]) : Map[String, Def] = {
  l match {
    case Nil => acc
    case first :: rest => first match {
      // case Def(name, args, exp) => interpProgHelp(rest, acc + (name -> Call(args, exp)))
      case Def(name, args, exp) => interpProgHelp(rest, acc + (name -> first))
      }
  }
}


 

def evalExp(e : String) : SExp = interpExp(parseExp(parseSExp(e)), Map(), Map())

def evalProgram(p : String) : SExp = interpProgram(parseProgram(parseSExp(p)))

/**

// Arithmetic tests
val arithEx1 = parseExp(parseSExp("(* (+ 5 2) 1)"))

// Let tests
val letEx1 = parseExp(parseSExp(
  """(let ((x 5))
       (+ x 7))"""))
assert(interpExp(letEx1, Map(), Map()) == SInt(12))

val letEx2 = parseExp(parseSExp(
  """(let ((x (+ 5 6)))
       (+ x 7))"""))
assert(interpExp(letEx2, Map(), Map()) == SInt(18))

val letEx3 = parseExp(parseSExp(
  """(let ((x (let ((y 1))
                 (+ y 2))))
        (+ x 3))"""))
assert(interpExp(letEx3, Map(), Map()) == SInt(6))

*/

// Program parsing tests. We'll represent a program
// as a sequence of defintions, followed by a main
val progEx1 = parseProgram(parseSExp(
"""
((define (square n)
   (* n n))
 (square 5))
"""))

assert(progEx1 ==
  Program(List(
    Def("square", "n", Multiply(Ref("n"), Ref("n")))
  ),
  Call("square", Literal(SInt(5))))
)

val progEx2 = parseProgram(parseSExp(
"""
((define (square n)
   (* n n))
 (define (cube n)
   (* n (square n)))
 (cube 5))
"""))

assert(progEx2 ==
  Program(List(
    Def("square", "n", Multiply(Ref("n"), Ref("n"))),
    Def("cube", "n", Multiply(Ref("n"), Call("square", Ref("n"))))
  ),
  Call("cube", Literal(SInt(5))))
)
