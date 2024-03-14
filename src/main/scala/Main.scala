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

  def main(args: Array[String]): Unit = {
    println("Hello world!")
    var g = validId(Symbol("then"));
    print(g)
  }
}