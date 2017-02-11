name := "textsearch"
version := "1.0"
scalaVersion := "2.11.7"
assemblyJarName in assembly := "textsearch.jar"
libraryDependencies ++= {
  Seq("org.specs2" %% "specs2" % "2.3.13" % "test")
}
