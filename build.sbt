scalaVersion := "2.10.2"

initialCommands in console := """import info.folone.Hanoi._, shapeless._, nat._"""

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.0.0-M1" cross CrossVersion.full
)
