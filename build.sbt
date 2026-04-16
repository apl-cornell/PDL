name := "PipelineDescriptionLanguage"
version := "0.0.1"
scalaVersion := "3.3.6"

libraryDependencies ++= Seq(
  "commons-io" % "commons-io" % "2.18.0",

  // Parsing & Pretty Printing
  "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0",
  "com.lihaoyi" %% "pprint" % "0.9.0",

  // SMT Solving
  "tools.aqua" % "z3-turnkey" % "4.13.0",

  // Command Line Parsing
  "com.github.scopt" %% "scopt" % "4.1.0",

  // Logging
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5",
  "ch.qos.logback" % "logback-classic" % "1.5.18",

  // Testing
  "org.scalatest" %% "scalatest" % "3.2.19" % "test",
  "org.scalactic" %% "scalactic" % "3.2.19",
)

scalacOptions ++= Seq("-language:implicitConversions", "-source:3.3-migration")

Test / classLoaderLayeringStrategy := ClassLoaderLayeringStrategy.Flat

//Deployment Options
assembly / assemblyJarName := "pdl.jar"
assembly / test := {}
assembly / mainClass := Some("pipedsl.Main")
assembly / assemblyMergeStrategy := {
  case "module-info.class" => MergeStrategy.discard
  case PathList("META-INF", "versions", _, "module-info.class") => MergeStrategy.discard
  case x => (assembly / assemblyMergeStrategy).value(x)
}
