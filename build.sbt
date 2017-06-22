name := "throb"

version := "0.1"

scalaVersion := "2.12.2"

libraryDependencies ++= Seq("org.specs2" %% "specs2-core" % "3.9.1" % "test")

scalacOptions := Seq("-feature", "-language:implicitConversions", "-language:postfixOps")
