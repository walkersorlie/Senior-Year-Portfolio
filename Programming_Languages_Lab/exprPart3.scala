import sexp._

sealed abstract class Exp
case class A(b : Box[SExp])	
case class B(a : A)
case class Call(lhs : Exp, rhs : List[Exp]) extends Exp
case class Closure(v : Lambda, e : Env) extends SExp
case class Def(name : String, args : List[String], body: Exp)
case class Equal(lhs: Exp, rhs: Exp) extends Exp
case class If(tf : Exp, truE : Exp, falE : Exp) extends Exp
case class Lambda(ids : List[String], body : Exp) extends Exp
case class Let(v : String, rhs : Exp, body : Exp) extends Exp
case class Primitive(name : String) extends SExp
case class Program(defs : List[Def], exp : Exp)
case class Quote(v : SExp) extends Exp
case class Ref(v : String) extends Exp

class Box[A](var contents : Option[A] = None)


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
    case SList(SSymbol("define"), SCons(SSymbol(first), rest), body) => Def(first, parseDefineHelp(rest, Nil), parseExp(body)) 
  } 
}

def parseDefineHelp(s : SExp, acc : List[String]) : List[String] = {
  s match {
    case SNil => acc.reverse
    case SCons(SSymbol(first), rest) => parseDefineHelp(rest, first :: acc)
  }
}

def parseExp(e : SExp) : Exp = { 
	e match {
  	case SInt(v) => Quote(SInt(v))
    case STrue() => Quote(STrue())
    case SFalse() => Quote(SFalse())
    case SList(SSymbol("let"), SList(SList(SSymbol(id), rhs)), body) => Let(id, parseExp(rhs), parseExp(body))
    case SList(SSymbol("if"), torf, te, fe) => If(parseExp(torf), parseExp(te), parseExp(fe))
    case SList(SSymbol("equal?"), l, r) => Equal(parseExp(l), parseExp(r))
		case SList(SSymbol("quote"), exp) => Quote(exp)
		case SList(SSymbol("lambda"), id, body) => Lambda(parseDefineHelp(id, List()), parseExp(body))
    case SCons(e1, e2) => Call(parseExp(e1), SList.toList(e2).get.map(parseExp _))
    case SSymbol(id) => Ref(id)
    case _ => throw new IllegalArgumentException("not a valid expression")
	}
}

type Env = Map[String, Box[SExp]]   

val initialEnv : Env = Map(
	"+" -> new Box(Some(Primitive("+"))),
  "-" -> new Box(Some(Primitive("-"))),
  "*" -> new Box(Some(Primitive("*"))),
  "null?" -> new Box(Some(Primitive("null?"))),
  "cons" -> new Box(Some(Primitive("cons"))),
  "car" -> new Box(Some(Primitive("car"))),
  "cdr" -> new Box(Some(Primitive("cdr"))),
  "pair?" -> new Box(Some(Primitive("pair?"))),
  "null" ->  new Box(Some(SNil))
)

def interpExp(e : Exp, env : Env) : SExp = {
	e match {
  	case Ref(id) => env.get(id) match {
    	case None => throw new RuntimeException("unbound reference variable: " + id)
    	case Some(v) => v.contents.get
    }
  	case Let(id, rhs, body) => {
      val box = new Box[SExp]()
      box.contents = Option(interpExp(rhs, env))
    	val newEnv = env + (id -> box)  
    	interpExp(body, newEnv)
    }
		case Call(exp1, eArgs) => { 
			val v1 = interpExp(exp1, env)
     	val vArgs = eArgs.map((e : Exp) => interpExp(e, env))
      	v1 match {
        	case Closure(Lambda(ids, body), cenv) => {
            interpExp(body, (interpCallHelp(ids, vArgs, cenv)) )
          }
          case Primitive("+") =>
						val SInt(arg0) = vArgs(0)
						val SInt(arg1) = vArgs(1)
            SInt(arg0 + arg1)
          case Primitive("-") =>
            val SInt(arg0) = vArgs(0)
            val SInt(arg1) = vArgs(1)
            SInt(arg0 - arg1)  
          case Primitive("*") =>
            val SInt(arg0) = vArgs(0)
						val SInt(arg1) = vArgs(1)
            SInt(arg0 * arg1)
					case Primitive("null?") => if (vArgs(0) == SNil) STrue() else SFalse()
					case Primitive("cons") => SCons(vArgs(0), vArgs(1))
					case Primitive("car") => vArgs(0) match {
      			case SCons(first, second) => first
        		case _ => throw new RuntimeException("not a pair")
					}
					case Primitive("cdr") => vArgs(0) match {
						case SCons(first, second) => second
						case _ => throw new RuntimeException("not a pair")
					}
					case Primitive("pair?") => vArgs(0) match {
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
		case Quote(v) => v
		case Lambda(id, body) => Closure(Lambda(id, body), env)
	}
}

 def interpCallHelp(l : List[String], v : List[SExp], finalEnv : Env) : Env = {
  (l, v) match {
    case (Nil, Nil) => finalEnv
    case (lFirst :: lRest, vFirst :: vRest) => {
      val box = new Box[SExp]()
      box.contents = Option(vFirst)
      interpCallHelp( lRest, vRest, (finalEnv + (lFirst -> box)) )
    }
    case _ =>throw new RuntimeException("variable not referenced")
  }
 }

def interpProgram(p : Program) : SExp = { 
  val Program(defs, exp) = p          
  val env =  interpProgHelp(defs, initialEnv)
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
		case Def(name, args, body) => env(name).contents = Some(Closure(Lambda(args, body), env))	
	}
}


def evalProgram(s : String) : SExp = {
  interpProgram(parseProgram(parseSExp(s)))
}
