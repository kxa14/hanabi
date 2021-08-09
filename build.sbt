name := "hanabi"

version := "0.1"

scalaVersion := "2.13.6"

//scalacOptions += "-Ypartial-unification"
addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")

val catsCoreVersion = "2.6.1"
val catsRetryVersion = "3.0.0"
val monocleVersion = "2.1.0"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsCoreVersion,
  "org.typelevel" %% "cats-effect" % "3.1.0",
  "com.github.cb372" %% "cats-retry" % catsRetryVersion,
  "com.github.julien-truffaut" %% "monocle-core" % monocleVersion,
  "com.github.julien-truffaut" %% "monocle-macro" % monocleVersion,
  "org.scalamock" %% "scalamock" % "5.1.0" % Test,
  "org.scalatest" %% "scalatest" % "3.1.0" % Test,
  "org.typelevel" %% "cats-effect-testing-scalatest" % "1.0.0" % Test
)
