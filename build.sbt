name := "hanabi"

version := "0.1"

scalaVersion := "2.13.1"

//scalacOptions += "-Ypartial-unification"
addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "2.3.0",
  "org.typelevel" %% "cats-effect" % "3.1.0",
  "org.scalamock" %% "scalamock" % "5.1.0" % Test,
  "org.scalatest" %% "scalatest" % "3.1.0" % Test,
  "org.typelevel" %% "cats-effect-testing-scalatest" % "1.0.0" % Test
)
