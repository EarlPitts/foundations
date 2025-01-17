import sbt._

object Dependencies {
  val kantanGeneric = "com.nrinaudo"      %% "kantan.csv"      % "0.7.0"
  val scalatest     = "org.scalatest"     %% "scalatest"       % "3.2.18"   % Test
  val scalacheck    = "org.scalatestplus" %% "scalacheck-1-17" % "3.2.18.0" % Test
  val kindProjector = "org.typelevel"     %% "kind-projector"  % "0.13.3" cross CrossVersion.binary
  val cats          = "org.typelevel"     %% "cats-core"       % "2.9.0"

}
