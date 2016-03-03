libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided",
  "org.spire-math" %% "spire" % "0.11.0"
)

scalaVersion := "2.11.7"

scalacOptions ++= Seq(
  "-Yinline-warnings",
  "-unchecked",
  "-feature",
  "-deprecation",
  "-optimize",
  "-language:implicitConversions",
  "-language:higherKinds",
  "-language:experimental.macros"
)
