import sbtcrossproject.CrossPlugin.autoImport.{CrossType, crossProject}

name:= "wurzel"
lazy val root= (project in file (".")).
  aggregate(pitJS,pitJVM).
  settings(scalaVersion:="2.12.9",
    publish:={},
    publishLocal:={},
    libraryDependencies:= libraryDependencies.value ++ Seq( "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
      "org.scala-lang.modules" %% "scala-xml" % "1.2.0")
  ).enablePlugins {ScalaJSPlugin}

lazy val pit = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Full).in(file(".")).
  settings(
    name:="dbdef",
    version:="0.9-SNAPSHOT",
    scalaVersion:="2.12.9",
    scalacOptions ++= Seq( "-deprecation"),
    scalaJSStage in Global := FastOptStage,
    libraryDependencies:= libraryDependencies.value ++ Seq( "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
     "org.scala-lang.modules" %% "scala-xml" % "1.2.0", "io.github.cquiroz" %%% "scala-java-locales" % "0.5.3-cldr31"),
    //publishArtifact in (Compile, packageDoc) := false,
    publishArtifact in packageDoc := false,
    sources in (Compile,doc) := Seq.empty
  ).
  jvmSettings().
  jsSettings(). enablePlugins(ScalaJSPlugin)
lazy val pitJVM = pit.jvm
lazy val pitJS = pit.js
