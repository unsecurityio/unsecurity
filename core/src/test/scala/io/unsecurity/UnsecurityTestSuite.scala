package io.unsecurity

import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite

trait UnsecurityTestSuite extends AnyFunSuite {
  sealed trait TestResult
  case object Ok                 extends TestResult
  case class Fail(error: String) extends TestResult

  implicit class Where[A](result: A) {
    def where(pf: PartialFunction[A, TestResult]): Assertion = {
      if !pf.isDefinedAt(result) then fail("not defined for: " + result.toString)
      else {
        pf(result) match {
          case Ok          => succeed
          case Fail(error) => fail(error)
        }
      }
    }
  }
}
