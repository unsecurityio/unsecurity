package io.unsecurity.auth.auth0.m2m

case class OauthAuthenticatedApplication(applicationId: ApplicationId, name: Option[String], scopes: List[String])

case class ApplicationId(asString: String)
