enablePlugins(ScalaJSPlugin)
lazy val root:Project= (project in file(".")).settings(
  name:="Calendar",
  version:="0.1",
  scalaVersion:="2.13.1",
  scalacOptions ++= Seq( "-deprecation"),
  scalaJSStage in Global := FastOptStage,
  artifactPath in (Compile,fastOptJS) := file("/media/platted/Programmdaten/Scalabase/deploy/Files/calendar.js"),
  artifactPath in (Compile,fullOptJS) := file("/media/platted/Programmdaten/Scalabase/deploy/Files/calendar.js")
)
libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.7"
libraryDependencies +=  "dbdef" %%% "dbdef" % "0.9-SNAPSHOT"
libraryDependencies += "jsbase" %%% "jsbase" % "0.1-SNAPSHOT"
libraryDependencies += "org.scala-lang.modules" %%% "scala-parser-combinators" % "1.1.2"
libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.8.2"
addCommandAlias("f", "fastOptJS")
scalaJSUseMainModuleInitializer := true