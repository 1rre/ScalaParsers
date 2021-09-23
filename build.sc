import mill._, scalalib._

trait WithDeprecation extends ScalaModule {
  def scalacOptions = Seq("-deprecation")
}

object Parsers extends WithDeprecation {
  def scalaVersion = "3.0.1"
}

object Test extends WithDeprecation {
  def scalaVersion = "3.0.1"
  def moduleDeps = Seq(Parsers)
}
object CParser extends WithDeprecation {
  def scalaVersion = "3.0.1"
  def moduleDeps = Seq(Parsers)
}

object Ir extends WithDeprecation {
  def scalaVersion = "2.13.6"
  def ivyDeps= Agg(ivy"org.scala-lang.modules::scala-parser-combinators:2.0.0")
}