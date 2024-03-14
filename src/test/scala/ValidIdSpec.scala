import Main.{hasDuplicates, validId}
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
}
