package unsecurity.auth.auth0.m2m

case class OauthAuthenticatedApplication(applicationId: ApplicationId, scopes: List[String], rawToken:String)

case class ApplicationId(value: String) extends AnyVal
