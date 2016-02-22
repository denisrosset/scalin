libraryDependencies += "org.spire-math" %% "spire" % "0.11.0"

scalaVersion := "2.11.7"

scalacOptions ++= Seq(
  "-unchecked",
  "-feature",
  "-deprecation",
  "-optimize",
  "-language:implicitConversions",
  "-language:higherKinds"
)
