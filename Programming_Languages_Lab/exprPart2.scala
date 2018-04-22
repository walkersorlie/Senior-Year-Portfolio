import sexp._

sealed abstract class Exp
case class Literal(v : SExp) extends Exp
case class Add(lhs : Exp, rhs : Exp) extends Exp
case class Multiply(lhs : Exp, rhs : Exp) extends Exp
case class Call(lhs : Exp, rhs : List[Exp]) extends Exp 
case class Let(v : String, rhs : Exp, body : Exp) extends Exp
case class Ref(v : String) extends Exp
case class If(tf : Exp, truE : Exp, falE : Exp) extends Exp
case class Equal(lhs: Exp, rhs: Exp) extends Exp
case class Def(name : String, args : List[String], body: Exp)
case class Program(defs : List[Def], exp : Exp)
case class Cons(e1 : Exp, e2 : Exp) extends Exp
case class CheckPair(v : Exp) extends Exp				
case class NullCheck(compare : Exp) extends Exp		
case class Car(car : Exp) extends Exp
case class Cdr(cdr : Exp) extends Exp
case class Quote(v : Exp) extends Exp

case class Lambda(id : String, body : Exp) extends Exp
case class Closures(v : Lambda, e : Env) extends SExp
case class Primitives(name : String) extends SExp

case class A(b : Box[A])		// not sure here
case class B(a : A)

class Box[A] {
  var contents : Option[A] = None
}


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
    case SList(SSymbol("define"), SCons(first, rest), body) => Def(first.toString, parseDefineHelp(rest, Nil), parseExp(body)) 
  } 
}

def parseDefineHelp(s : SExp, acc : List[String]) : List[String] = {
  s match {
    case SNil => acc.reverse		// reverse here because the name should be the first thing, and without reverse it is the last
    case SCons(first, rest) => parseDefineHelp(rest, first.toString :: acc)
  }
}

def parseExp(e : SExp) : Exp = { 
	e match {
  	case SInt(v) => Literal(SInt(v))
    case STrue() => Literal(STrue())
    case SFalse() => Literal(SFalse())
    case SNil => Literal(SNil)
    case SList(SSymbol("+"), l, r) => Add(parseExp(l), parseExp(r))
    case SList(SSymbol("*"), l, r) => Multiply(parseExp(l), parseExp(r))
    case SSymbol(id) => Ref(id)
    case SList(SSymbol("let"), SList(SList(SSymbol(id), rhs)), body) => Let(id, parseExp(rhs), parseExp(body))
//    case SList(id, SCons(first, rest)) => Call(parseExp(id), (rest.toList).map((arg : SExp) => parseExp(arg)))
		case SCons(e1, e2) => Call(parseExp(e1), e2.asInstanceOf[SCons].toList.map(parseExp _)) 
    case SList(SSymbol("if"), torf, te, fe) => If(parseExp(torf), parseExp(te), parseExp(fe))
    case SList(SSymbol("equal?"), l, r) => Equal(parseExp(l), parseExp(r))
		case SList(SSymbol("cons"), car, cdr) => Cons(parseExp(car), parseExp(cdr))
		case SList(SSymbol("car"), car) => Car(parseExp(car))
		case SList(SSymbol("cdr"), cdr) => Cdr(parseExp(cdr))
		case SList(SSymbol("pair?"), exp) => CheckPair(parseExp(exp))
    case SList(SSymbol("null")) => Literal(SNil)		// SSymbol("null") => Literal(SNil)
		case SList(SSymbol("null?"), v) => NullCheck(parseExp(v))	
		case SList(SSymbol("quote"), exp) => Quote(Literal(exp))
		case SList(SSymbol("lambda"), SSymbol(id), body) => Lambda(id, parseExp(body))		// not sure here
    case _ => throw new IllegalArgumentException("not a valid expression")
	}
}

type Env = Map[String, Box[SExp]]   

def interpExp(e : Exp, env : Env) : SExp = {
	e match {
  	case Literal(v) => {
    	v match {
    	  case SInt(v2) => SInt(v2)
			  case STrue() => STrue() 
			  case SFalse() => SFalse()
    	}
  	} 
  	case Ref(id) => env.get(id) match {
    	case None => throw new RuntimeException("unbound variable")
    	case Some(v) => v.contents.get
    }
  	case Let(id, rhs, body) => {
    	val rhsVal = interpExp(rhs, env)
			val box = new Box[SExp]
			box.contents = Some(rhsVal)
    	val newEnv = env + (id -> box)
    	interpExp(body, newEnv)
    }
		case Call(exp1, eArgs) => { 
			val v1 = interpExp(exp1, env)
     	val vArgs = eArgs.map((e : Exp) => interpExp(e, env))
      	v1 match {
        	case Closures(Lambda(id, body), env2) => {
          	val List(v2) = vArgs
						val box = new Box[SExp]
						box.contents = Some(v2)
            val newEnv = env2 + (id -> box)
            interpExp(body, newEnv)
          }
          case Primitives("+") =>
						val SInt(arg0) = vArgs(0)
						val SInt(arg1) = vArgs(1)
            SInt(arg0 + arg1)
          case Primitives("*") =>
            val SInt(arg0) = vArgs(0)
						val SInt(arg1) = vArgs(1)
            SInt(arg0 * arg1)
					case Primitives("null?") => if (vArgs(0) == SNil) STrue() else SFalse()
					case Primitives("cons") => SCons(vArgs(0), vArgs(1))
					case Primitives("car") => vArgs(0) match {
      			case SCons(first, second) => first
        		case _ => throw new RuntimeException("not a pair")
					}
					case Primitives("cdr") => vArgs(0) match {
						case SCons(first, second) => second
						case _ => throw new RuntimeException("not a pair")
					}
					case Primitives("pair?") => vArgs(0) match {
						case SCons(first, rest) => STrue()
						case _ => SFalse()
					}
          case _ => throw new RuntimeException("tried to call a non-function")
			}
		}

  	case If(torf, tex, fex) => {
    	val tf = interpExp(torf, env)
    	tf match {
      	case SFalse() => interpExp(fex, env)
      	case _ => interpExp(tex, env)
    	}
  	}
  	case Equal(l, r) => {	
    	val e1 = interpExp(l, env)
    	val e2 = interpExp(r, env)
    	if(e1 == e2) {
    	  STrue()
    	}
    	else {
    	  SFalse()
    	}
  	}
		case Quote(v) => SSymbol(v.toString)
		case Lambda(id, body) => Closures(Lambda(id, body), env)
	}
}

def interpProgram(p : Program) : SExp = {  
  val Program(defs, exp) = p          
  val env =  interpProgHelp(defs, Map())
	defs.foreach((defs : Def) => closureHelp(defs, env))
  interpExp(exp, env)           
}

def interpProgHelp(l : List[Def], acc : Map[String, Box[SExp]]) : Map[String, Box[SExp]] = {
	l match {
		case Nil => acc
		case first :: rest => first match {
			case Def(name, args, exp) => interpProgHelp(rest, acc + (name -> new Box[SExp]()))
		}
	}
}

def closureHelp(defs : Def, env : Map[String, Box[SExp]]) : Unit = {
	defs match {
		case Def(name, args, body) => env(name).contents = Some(Closures(Lambda(name, body), env))	
	}
}
