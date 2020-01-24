package io.unsecurity.auth.auth0

import java.util.UUID

import cats.effect.IO
import com.auth0.jwk.{Jwk, JwkProvider}
import io.unsecurity.auth.auth0.m2m.Auth0M2MSecurityContext
import no.scalabin.http4s.directives.Result
import org.http4s.{Request, _}

class Auth0M2MSecurityContextTest extends UnsecurityTestSuite{

  val m2mSecurity = new Auth0M2MSecurityContext[IO, Nothing](_ => ???, "issuer", "aud", new FdUpJwkProvider)

  test("Identifies Bearer token when multiple Authorization headers set") {
    val get = Request[IO](headers = Headers.of(Header("Authorization", "M2M Don't say you love me, you don't even know me"), Header("Authorization", s"Bearer ${UUID.randomUUID().toString}")))
    m2mSecurity.requestAuthToken.run(get).unsafeRunSync().where{
      case Result.Success(token) => Ok
    }
  }

  class FdUpJwkProvider extends JwkProvider {
    override def get(keyId: String): Jwk = ???
  }

}
