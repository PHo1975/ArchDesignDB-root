

name:= "wurzel"
lazy val root= (project in file (".")).
  aggregate(pitJS,pitJVM).
  settings(scalaVersion:="2.13.4",
    publish:={},
    publishLocal:={},
    libraryDependencies:= libraryDependencies.value ++ Seq( "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
      "org.scala-lang.modules" %% "scala-xml" % "1.2.0")
  ).enablePlugins {ScalaJSPlugin}

lazy val pit = _root_.sbtcrossproject.CrossPlugin.autoImport.crossProject(JSPlatform, JVMPlatform).crossType(
  _root_.sbtcrossproject.CrossType.Full).in(file(".")).
  settings(
    name:="dbdef",
    version:="0.9-SNAPSHOT",
    scalaVersion:="2.13.4",
    scalacOptions ++= Seq( "-deprecation"),
    scalaJSStage in Global := FastOptStage,
    libraryDependencies:= libraryDependencies.value ++ Seq( "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
     "org.scala-lang.modules" %% "scala-xml" % "1.2.0"/*, "io.github.cquiroz" %%% "scala-java-locales" % "0.3.16-cldr35"*/),
    //publishArtifact in (Compile, packageDoc) := false,
    publishArtifact in packageDoc := false,
    sources in (Compile,doc) := Seq.empty
  ).
  jvmSettings().
  jsSettings(). enablePlugins(ScalaJSPlugin)
lazy val pitJVM = pit.jvm
lazy val pitJS = pit.js
