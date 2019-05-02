package io.unsecurity.auth.auth0.m2m

case class OauthAuthenticatedApplication(applicationId: ApplicationId, scopes: List[String])

case class ApplicationId(asString: String) extends AnyVal
