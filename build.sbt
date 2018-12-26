
name:= "wurzel"
lazy val root= (project in file (".")).
  aggregate(pitJS,pitJVM).
  settings(scalaVersion:="2.12.4",
    publish:={},
    publishLocal:={},
    libraryDependencies:= libraryDependencies.value ++ Seq( "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.0",
      "org.scala-lang.modules" %% "scala-xml" % "1.0.6")
  ).enablePlugins {
  ScalaJSPlugin
}

lazy val pit = crossProject.in(file(".")).
  settings(
    name:="dbdef",
    version:="0.9-SNAPSHOT",
    scalaVersion:="2.12.4",
    scalacOptions ++= Seq( "-deprecation"),
    scalaJSStage in Global := FastOptStage,
    libraryDependencies:= libraryDependencies.value ++ Seq( "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.0",
     "org.scala-lang.modules" %% "scala-xml" % "1.0.6"),
    publishArtifact in (Compile, packageDoc) := false,
    publishArtifact in packageDoc := false,
    sources in (Compile,doc) := Seq.empty
  ).
  jvmSettings().
  jsSettings() enablePlugins(ScalaJSPlugin)
lazy val pitJVM = pit.jvm
lazy val pitJS = pit.js
