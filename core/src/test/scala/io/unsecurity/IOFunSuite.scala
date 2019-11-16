package io.unsecurity

import cats.effect.IO
import org.scalatest.{Assertion, AsyncFunSuite}

class IOFunSuite extends AsyncFunSuite {
  object test {
    def update(name:String, a:IO[Assertion]) =
      test(name){ a.unsafeToFuture() }
  }

  /*
    test("hello") = for {
      x <- IO(1)
    } yield assert(x == 1)
  */

}
