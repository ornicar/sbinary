organization := "org.scala-tools.sbinary"

version := "0.4.1-paulp-SNAPSHOT"

scalaVersion := "2.9.0"

crossScalaVersions += "2.8.1"

// logBuffered in Test in ThisBuild := false

testOptions in Test in ThisBuild += Tests.Argument("-s", System.getProperty("sbinary.tests"))

parallelExecution in Test in ThisBuild := false
