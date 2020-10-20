name := "PipeDSL"
version := "0.0.1"
scalaVersion := "2.13.2"

val osInf = Option(System.getProperty("os.name")).getOrElse("")
val arcInf = System.getProperty("os.arch")

val osArch = if(arcInf.indexOf("64") >= 0) "x64" else "x86"
val isUnix = osInf.indexOf("nix") >= 0 || osInf.indexOf("nux") >= 0
val isWindows = osInf.indexOf("Win") >= 0
val isMac = osInf.indexOf("Mac") >= 0

val osName = if (isWindows) "win" else if (isMac) "mac" else "unix"

Compile / unmanagedJars += {
  baseDirectory.value / "unmanaged" / s"scalaz3-$osName-$osArch-${scalaBinaryVersion.value}.jar"
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