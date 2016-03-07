libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided",
  "org.spire-math" %% "spire" % "0.11.0",
  "org.scalatest" %% "scalatest" % "3.0.0-M7" % "test"
)

scalaVersion := "2.11.7"

scalacOptions ++= Seq(
//  "-Xlog-implicits",
  "-Yinline-warnings",
  "-unchecked",
  "-feature",
  "-deprecation",
  "-optimize",
  "-language:implicitConversions",
  "-language:higherKinds",
  "-language:experimental.macros"
)

tutSettings
