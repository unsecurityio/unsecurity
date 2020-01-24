package io.unsecurity.auth.auth0

import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite

trait UnsecurityTestSuite extends AnyFunSuite {
  sealed trait TestResult
  case object Ok                 extends TestResult
  case class Fail(error: String) extends TestResult

  implicit class Where[A](a: A) {
    def where(pf: PartialFunction[A, TestResult]): Assertion = {
      assertResult(a, pf)
    }

    private def assertResult(result: A, pf: PartialFunction[A, TestResult]) = {
      if (!pf.isDefinedAt(result)) fail("not defined for: " + result.toString)
      else {
        val r: TestResult = pf(result)

        r match {
          case Ok          => succeed
          case Fail(error) => fail(error)
        }
      }
    }
  }
}
