object Main {

  sealed trait ExprC

  case class NumC(n: Double) extends ExprC
  case class IdC(vr: Symbol) extends ExprC
  case class AppC(fun: ExprC, args: List[ExprC]) extends ExprC
  case class StringC(str: String) extends ExprC
  case class IfC(test: ExprC, thenn: ExprC, elsee: ExprC) extends ExprC
  case class LetC(bindings: List[Binding], body: ExprC) extends ExprC
  case class AnonC(params: List[Symbol], body: ExprC) extends ExprC
  case class LamC(args: List[Symbol], body: ExprC) extends ExprC
  case class DummyC() extends ExprC // Added to mimic the dummyC

  sealed trait Value

  case class NumV(v: Double) extends Value
  case class BoolV(v: Boolean) extends Value
  case class StrV(v: String) extends Value
  case class CloV(args: List[Symbol], body: ExprC, env: Env) extends Value
  case class ErrorV(msg: String) extends Value
  case class PrimOp(func: List[Value] => Value) extends Value
  case class PrimFun(func: Value => Value) extends Value
  case class PrimFunNoArg(func: () => Value) extends Value
  case class DummyV() extends Value // Added to mimic the dummyV

  case class Binding(name: Symbol, v: Value)
  type Env = List[Binding]

  /*
    (define (validId? [id : Symbol]) : Boolean
    (match id
      ['if #f]
      ['then #f]
      ['else #f]
      ['let #f]
      ['anon #f]
      [': #f]
      ['<- #f]
      [_ #t]))
  */
  def validId(id: Symbol): Boolean = id match {
    case Symbol("if") | Symbol("then") | Symbol("else") | Symbol("let") | Symbol("anon") | Symbol(":") | Symbol("<-") => false
    case _ => true
  }


  /*
  ;; Checks if a list has any duplicate symbols
  (define (has-duplicates? [syms : (Listof Symbol)]) : Boolean
    (match syms
      ['() #f]
      [(cons first rest)
       (or (not (equal? (member first rest) #f))
           (has-duplicates? rest))]))
   */
  def hasDuplicates(syms: List[Symbol]): Boolean = syms match {
    case Nil => false
    case head :: tail => tail.contains(head) || hasDuplicates(tail)
  }


  /*
  ;; Checks if list is all symbols
  (define (all-symbols? [s : (Listof Any)]) : Boolean
    (match s
      ['() #t]
      [(cons f r)
       (and (symbol? f) (all-symbols? r))]))
   */
  def allSymbols(s: List[Any]): Boolean = s match {
    case Nil => true
    case (head: Symbol) :: tail => allSymbols(tail)
    case _ => false
  }

  def lookup(id: Symbol, env: Env): Value = env match {
    case Nil => throw new Exception(s"OAZO: Name not found $id")
    case Binding(sym, value) :: rest =>
      if (sym == id) value
      else lookup(id, rest)
  }

  def extend(params: List[Symbol], argsEval: List[Value], env: Env): Env = params.zip(argsEval).map {
    case (param, argVal) => Binding(param, argVal)
  } ::: env

  def addop(args: List[Value]): Value = args match {
    case NumV(a) :: NumV(b) :: Nil => NumV(a + b)
    case _ => throw new Exception("OAZO: + expects 2 numbers")
  }

  def subop(args: List[Value]): Value = args match {
    case NumV(a) :: NumV(b) :: Nil => NumV(a - b)
    case _ => throw new Exception("OAZO: - expects 2 numbers")
  }

  def mulop(args: List[Value]): Value = args match {
    case NumV(a) :: NumV(b) :: Nil => NumV(a * b)
    case _ => throw new Exception("OAZO: * expects 2 numbers")
  }

  def divop(args: List[Value]): Value = args match {
    case NumV(a) :: NumV(b) :: Nil =>
      if (b != 0) NumV(a / b)
      else throw new Exception("OAZO: Division by zero")
    case _ => throw new Exception("OAZO: / expects 2 numbers")
  }

  def ifleqop(args: List[Value]): Value = args match {
    case NumV(a) :: NumV(b) :: Nil => BoolV(a <= b)
    case _ => throw new Exception("OAZO: <= expects 2 numbers")
  }

  def equalop(args: List[Value]): Value = args match {
    case NumV(a) :: NumV(b) :: Nil => BoolV(a == b)
    case StrV(a) :: StrV(b) :: Nil => BoolV(a == b)
    case BoolV(a) :: BoolV(b) :: Nil => BoolV(a == b)
    case _ => BoolV(false)
  }

  def errorop(args: List[Value]): Value = args match {
    case v :: Nil => throw new Exception(s"OAZO: user-error ${v.toString}")
    case _ => throw new Exception("OAZO: error expects a single argument")
  }

  // Define topEnv
  val topEnv: Env = List(
    Binding(Symbol("+"), PrimOp(addop)),
    Binding(Symbol("-"), PrimOp(subop)),
    Binding(Symbol("*"), PrimOp(mulop)),
    Binding(Symbol("/"), PrimOp(divop)),
    Binding(Symbol("<="), PrimOp(ifleqop)),
    Binding(Symbol("equal"), PrimOp(equalop)),
    Binding(Symbol("true"), BoolV(true)),
    Binding(Symbol("false"), BoolV(false)),
    Binding(Symbol("error"), PrimOp(errorop)),
    Binding(Symbol("x"), NumV(3)),
  )


  def interp(exp: ExprC, env: Env): Value = exp match {
    case NumC(n) => NumV(n) // tested
    case StringC(s) => StrV(s) // tested
    case IfC(test, thenn, elsee) =>
      interp(test, env) match {
        case BoolV(true) => interp(thenn, env) // tested
        case BoolV(false) => interp(elsee, env) // tested
        case _ => throw new RuntimeException("OAZO: Test did not return a boolean") // tested
      }
    case IdC(id) => lookup(id, env) //tested
    case LamC(args, body) => CloV(args, body, env) // tested
    case AnonC(args, body) => CloV(args, body, env) // tested
    case AppC(fun, args) => // tested
      interp(fun, env) match {
        case CloV(params, body, closureEnv) =>
          if (params.length != args.length) {
            throw new RuntimeException("OAZO: Incorrect number of arguments") // tested
          } else {
            val argsEval = args.map(arg => interp(arg, env))
            interp(body, extend(params, argsEval, closureEnv)) // tested
          }
        case PrimOp(f) => f(args.map(arg => interp(arg, env))) // tested
        case _ => throw new RuntimeException("OAZO: Not a function") // tested
      }
    case _ => throw new RuntimeException("OAZO: Invalid expression") // tested
  }


  def main(args: Array[String]): Unit = {
    println("Hello world!")
    var g = validId(Symbol("then"));
    print(g)
  }
}