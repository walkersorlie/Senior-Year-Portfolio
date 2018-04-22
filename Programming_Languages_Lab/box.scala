import sexp._

sealed abstract class Exp
case class Literal(v: Int) extends Exp
case class Add(lhs: Exp, rhs: Exp) extends Exp
case class Multiply(lhs: Exp, rhs: Exp) extends Exp
case class Let(v : String, rhs: Exp, body : Exp) extends Exp
case class Ref(v : String) extends Exp
case class Lambda(id: String, body: Exp) extends Exp
case class Call(e1 : Exp, e2: Exp) extends Exp

def parseExp(e: SExp) : Exp = {
    e match {
      case SInt(v) => Literal(v)
      case SList(SSymbol("+"), l, r) => Add(parseExp(l), parseExp(r))
      case SList(SSymbol("*"), l, r) => Multiply(parseExp(l), parseExp(r))
      case SSymbol(id) => Ref(id)
      case SList(SSymbol("let"), SList(SList(SSymbol(id), rhs)), body) => Let(id, parseExp(rhs), parseExp(body))
      case SList(SSymbol("lambda"), SList(SSymbol(id)), body) => Lambda(id, parseExp(body))
      case SList(e1, e2) => Call(parseExp(e1), parseExp(e2))
      case _ => throw new IllegalArgumentException("not a valid expression")
    }
}

type Env = Map[String, Val]

abstract class Val
case class VInt(v : Int) extends Val
case class Procedure(v : Lambda, e : Env) extends Val
case class Primitive(name : String) extends Val 

class Box[Env] {
  var contents : Option[Env] = None
} 

def interpExp(e: Exp, env : Env) : Val = {
    e match {
        case Literal(v) => VInt(v)
        case Add(l,r) => {
          val VInt(lv) = interpExp(l, env)
          val VInt(rv) = interpExp(r, env)
          VInt(lv + rv)
        }
        case Multiply(l,r) => {
          val VInt(lv) = interpExp(l, env)
          val VInt(rv) = interpExp(r, env)
          VInt(lv * rv)
        }
        case Call(e1, e2) => {
            val v1 = interpExp(e1, env)
            val v2 = interpExp(e2, env)
            v1 match {
              case Procedure(Lambda(id, body), env2) => {
                val newEnv = env2 + (id -> v2)
                interpExp(body, newEnv)
              }
              case _ => throw new RuntimeException("tried to call a non-function")
            }
        }
        case Lambda(id, body) => Procedure(Lambda(id, body), env)
        case Ref(id) => env.get(id) match {
          case None => throw new RuntimeException("unbound variable")
          case Some(v) => v
        }
        case Let(id, rhs, body) =>
            val rhsVal = interpExp(rhs, env)
            val newEnv = env + (id -> rhsVal)
            interpExp(body, newEnv)
    }
}

def evalExp(e : String) : Val =
  interpExp(parseExp(parseSExp(e)), Map())


// Arithmetic tests
assert(evalExp("(* (+ 5 2) 1)") == VInt(7))

// Let tests
assert(evalExp(
  """(let ((x 5))
       (+ x 7))""")
     == VInt(12))

assert(evalExp(
  """(let ((x (+ 5 6)))
       (+ x 7))""")
 == VInt(18))

assert(evalExp(
  """(let ((x (let ((y 1))
                 (+ y 2))))
        (+ x 3))""")
    == VInt(6))

assert(evalExp(
  """
  (let ((double (lambda (n) (* n 2))))
    (double 5))
  """) == VInt(10))

// We'll make this work Wednesday.
assert(evalExp(
  """
  (let ((two 2))
    (let ((double (lambda (n) (* n two))))
      (double 5)))
  """) == VInt(10))


assert(evalExp(
  """
  (((lambda (x) (lambda (y) x))
    5)
   6)
  """)
  == VInt(5))

assert(evalExp(
  """
  (let ((twice (lambda (f) (lambda (arg) (f (f arg))))))
    ((twice (lambda (x) (* x 2))) 5))
  """)
  == VInt(20))

// vim: set ts=2 sw=2 et:
