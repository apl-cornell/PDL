name := "PipeDSL"
version := "0.0.1"
scalaVersion := "2.13.2"

Compile / unmanagedJars += {
  baseDirectory.value / "unmanaged" / "scalaz3-mac-x64-2.13.jar"
}

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.8" % "test",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "com.lihaoyi" %% "pprint" % "0.5.6",
  "com.github.scopt" % "scopt_2.13" % "4.0.0-RC2",
  "commons-io" % "commons-io" % "2.8.0"
)

//Deployment Options
assemblyJarName in assembly := "pdsl.jar"
test in assembly := {}
mainClass in assembly := Some("pipedsl.Main")