import mill._, scalalib._

object Parsers extends ScalaModule {
  def scalaVersion = "3.0.1"
}

object Test extends ScalaModule {
  def scalaVersion = "3.0.1"
  def moduleDeps = Seq(
    Parsers
  )
}