name := "PipelineDescriptionLanguage"
version := "0.0.1"
scalaVersion := "2.13.2"

libraryDependencies ++= Seq(
  "commons-io" % "commons-io" % "2.8.0",

  // Parsing & Pretty Printing
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
  "com.lihaoyi" %% "pprint" % "0.5.6",

  // SMT Solving
  "io.github.tudo-aqua" % "z3-turnkey" % "4.8.7.1",

  // Command Line Parsing
  "com.github.scopt" % "scopt_2.13" % "4.0.0-RC2",

  // Logging
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2",
  "ch.qos.logback" % "logback-classic" % "1.2.3",

  // Testing
  "org.scalatest" %% "scalatest" % "3.2.2" % "test",
  "org.scalactic" %% "scalactic" % "3.2.2",
)

scalacOptions += "-language:implicitConversions"

//Deployment Options
assemblyJarName in assembly := "pdl.jar"
test in assembly := {}
mainClass in assembly := Some("pipedsl.Main")
