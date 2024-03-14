import Main.{AnonC, AppC, Binding, BoolV, CloV, Env, ErrorV, IdC, IfC, LamC, NumC, NumV, PrimOp, StrV, StringC, addop, allSymbols, divop, equalop, errorop, hasDuplicates, ifleqop, interp, mulop, subop, validId}
import jdk.internal.vm.vector.VectorSupport.test
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.{a, be, include, the}
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class ValidIdSpec extends  AnyFunSuite {

  test("validId returns false for reserved keywords") {
    assert(!validId(Symbol("if")))
    assert(!validId(Symbol("then")))
    assert(!validId(Symbol("else")))
  }

  test("validId returns true for valid identifiers") {
    assert(validId(Symbol("myVar")))
    assert(validId(Symbol("anotherVar")))
  }

  test("hasDuplicates returns false for an empty list") {
    assert(!hasDuplicates(List()))
  }

  test("hasDuplicates returns false for a list with no duplicates") {
    assert(!hasDuplicates(List(Symbol("a"), Symbol("b"), Symbol("c"))))
  }

  test("hasDuplicates returns true for a list with duplicates at the beginning") {
    assert(hasDuplicates(List(Symbol("a"), Symbol("a"), Symbol("b"), Symbol("c"))))
  }

  test("hasDuplicates returns true for a list with duplicates at the end") {
    assert(hasDuplicates(List(Symbol("a"), Symbol("b"), Symbol("c"), Symbol("c"))))
  }

  test("hasDuplicates returns true for a list with duplicates at the middle") {
    assert(hasDuplicates(List(Symbol("a"), Symbol("c"), Symbol("c"), Symbol("b"))))
  }

  test("allSymbols should return true for an empty list") {
    allSymbols(List.empty) should be(true)
  }

  test("allSymbols should return true for a list containing only symbols") {
    allSymbols(List(Symbol("a"), Symbol("b"), Symbol("c"))) should be(true)
  }

  test("allSymbols should return false for a list containing a mix of symbols and other types") {
    allSymbols(List(Symbol("a"), Symbol("b"), "not a symbol", Symbol("c"))) should be(false)
  }

  test("allSymbols should return false for a list containing no symbols") {
    allSymbols(List(1, 2, 3)) should be(false)
  }

  test("allSymbols should return false for a list containing a single non-symbol") {
    allSymbols(List("not a symbol")) should be(false)
  }

  test("interp NumC to NumV") {
    val numExpr = NumC(42.0)
    interp(numExpr, List.empty) should be(NumV(42.0))
  }

  test("interp strExpr to StrV") {
    val strExpr = StringC("hello world")
    interp(strExpr, List.empty) should be(StrV("hello world"))
  }

  test("interp handles IfC with true condition") {
    val expr = IfC(IdC(Symbol("true")), StringC("then branch"), StringC("else branch"))
    interp(expr, Main.topEnv) should be(StrV("then branch"))
  }

  test("interp handles IfC with false condition") {
    val expr = IfC(IdC(Symbol("false")), StringC("then branch"), StringC("else branch"))
    interp(expr, Main.topEnv) should be(StrV("else branch"))
  }

  test("interp throws an error if IfC test does not return a boolean") {
    val ifExpr = IfC(NumC(1), StringC("then branch"), StringC("else branch"))
    val thrown = the[RuntimeException] thrownBy {
      interp(ifExpr, Main.topEnv)
    }
    thrown.getMessage should include("OAZO: Test did not return a boolean")
  }

  //AppC Tests
  test("AppC throws error when the number of arguments does not match") {
    val func = LamC(List(Symbol("x")), IdC(Symbol("x")))
    val app = AppC(func, List(NumC(1.0), NumC(2.0)))

    val exception = intercept[RuntimeException] {
      Main.interp(app, Main.topEnv)
    }
    exception.getMessage should be("OAZO: Incorrect number of arguments")
  }

  test("AppC throws error when the function is not a CloV or PrimOp") {
    val notAFunction = NumC(42.0)
    val app = AppC(notAFunction, List())

    val exception = intercept[RuntimeException] {
      Main.interp(app, Main.topEnv)
    }
    exception.getMessage should be("OAZO: Not a function")
  }

  test("AppC throws error when the expression is not valid") {
    val invalidApp = AppC(null, List())
    val exception = intercept[RuntimeException] {
      Main.interp(invalidApp, Main.topEnv)
    }
    exception.getMessage should be("OAZO: Invalid expression")
  }

  test("AppC evaluates function body with correct environment when CloV") {
    val add = IdC(Symbol("+"))
    val addapp = AppC(add, List(IdC(Symbol("x")) ,NumC(3.0)))
    val result = Main.interp(addapp, Main.topEnv)
    result should be(NumV(6.0))
  }

  test("AppC evaluates primitive operation") {
    val add = IdC(Symbol("+"))
    val app = AppC(add, List(NumC(5.0), NumC(3.0)))
    val result = Main.interp(app, Main.topEnv)
    result should be(NumV(8.0))
  }

  test("AppC evaluates function application with no arguments") {
    val func = LamC(List(), NumC(42.0))
    val app = AppC(func, List())
    val result = Main.interp(app, Main.topEnv)
    result should be(NumV(42.0))
  }

  //Anon tests
  test("interp creates a CloV from an AnonC expression") {
    val body = NumC(1.0)
    val anon = AnonC(List(Symbol("x")), body)
    val result = Main.interp(anon, Main.topEnv)
    result shouldBe a[CloV]
  }

  //LamC Tests
  test("interp creates a CloV from a LamC expression") {
    val body = NumC(1.0)
    val lam = LamC(List(Symbol("x")), body)
    val result = Main.interp(lam, Main.topEnv)
    result shouldBe a[CloV]
  }

  test("AppC evaluates function body with correct environment and argument") {
    val lam = LamC(List(Symbol("y")), IdC(Symbol("y")))
    val app = AppC(lam, List(IdC(Symbol("x"))))
    val result = Main.interp(app, Main.topEnv)
    result should be(NumV(3.0))
  }

}
