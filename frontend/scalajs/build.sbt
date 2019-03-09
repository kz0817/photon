enablePlugins(ScalaJSPlugin)

name := "Photon"
//scalaVersion := "2.12.6" // or any other Scala version >= 2.10.2

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.6"

// This is an application with a main method
scalaJSUseMainModuleInitializer := true

