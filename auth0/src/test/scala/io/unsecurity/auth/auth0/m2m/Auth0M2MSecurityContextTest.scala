package io.unsecurity.auth.auth0.m2m

import java.util.UUID

import cats.effect.IO
import com.auth0.jwk.{Jwk, JwkProvider}
import io.unsecurity.auth.auth0.UnsecurityTestSuite
import no.scalabin.http4s.directives.Result
import org.http4s.{Request, _}

class Auth0M2MSecurityContextTest extends UnsecurityTestSuite {

  val m2mSecurity = new Auth0M2MSecurityContext[IO, Nothing](_ => ???, "issuer", "aud", new FdUpJwkProvider)

  test("Extracts Bearer token from Authorization header") {
    val get = Request[IO](headers = Headers.of(Header("Authorization", s"Bearer ${UUID.randomUUID().toString}")))
    m2mSecurity.requestAuthToken.run(get).unsafeRunSync().where {
      case Result.Success(token) => Ok
      case Result.Error(e)       => Fail(e.bodyText.compile.lastOrError.unsafeRunSync())
    }
  }

  class FdUpJwkProvider extends JwkProvider {
    override def get(keyId: String): Jwk = ???
  }

}
