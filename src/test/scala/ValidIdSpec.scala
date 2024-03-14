import Main.validId
import jdk.internal.vm.vector.VectorSupport.test
import org.scalatest.funsuite.AnyFunSuite

class ValidIdSpec extends  AnyFunSuite {

  test("validId returns false for reserved keywords") {
    assert(!validId(Symbol("if")))
    assert(!validId(Symbol("then")))
    assert(!validId(Symbol("else")))
    // Add more assertions as needed
  }

  test("validId returns true for valid identifiers") {
    assert(validId(Symbol("myVar")))
    assert(validId(Symbol("anotherVar")))
    // Add more assertions as needed
  }
}
