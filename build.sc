import mill._, scalalib._

object Parsers extends ScalaModule {
  def scalaVersion = "3.0.1"
}

object Test extends ScalaModule {
  def scalaVersion = "3.0.1"
  def moduleDeps = Seq(Parsers)
}
object CParser extends ScalaModule {
  def scalaVersion = "3.0.1"
  def moduleDeps = Seq(Parsers)
}

object Ir extends ScalaModule {
  def scalaVersion = "2.13.6"
  def ivyDeps= Agg(ivy"org.scala-lang.modules::scala-parser-combinators:2.0.0")
}