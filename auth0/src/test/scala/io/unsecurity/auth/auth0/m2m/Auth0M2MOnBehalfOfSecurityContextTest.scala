package io.unsecurity.auth.auth0.m2m

import java.util.UUID

import cats.effect.IO
import com.auth0.jwk.{Jwk, JwkProvider}
import io.unsecurity.auth.auth0.UnsecurityTestSuite
import no.scalabin.http4s.directives.Result
import org.http4s.{Header, Headers, Request}

class Auth0M2MOnBehalfOfSecurityContextTest extends UnsecurityTestSuite {

  val m2mSecurity = new Auth0M2MOnBehalfOfSecurityContext[IO, Nothing](_ => ???, "issuer", "aud", new FdUpJwkProvider)

  test("Can extract both bearer and on-behalf-of authorization headers") {
    val get = Request[IO](
      headers = Headers.of(Header("Authorization", s"Bearer ${UUID.randomUUID().toString}"), Header("On-Behalf-Of", {
        UUID.randomUUID().toString
      })))
    m2mSecurity.requestAuthBearerToken.run(get).unsafeRunSync().where {
      case Result.Success(token) => Ok
      case Result.Error(e)       => Fail(e.bodyAsText.compile.lastOrError.unsafeRunSync())
    }

    m2mSecurity.requestOnBehalfOfToken.run(get).unsafeRunSync().where {
      case Result.Success(token) => Ok
    }
  }

  class FdUpJwkProvider extends JwkProvider {
    override def get(keyId: String): Jwk = ???
  }

}
