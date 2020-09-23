name := "PipeDSL"
version := "0.0.1"
scalaVersion := "2.13.2"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.8" % "test",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "com.lihaoyi" %% "pprint" % "0.5.6",
  "com.github.scopt" % "scopt_2.13" % "4.0.0-RC2"
)