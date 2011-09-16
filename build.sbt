organization := "org.scala-tools.sbinary"

version := "0.4.1-SNAPSHOT"

scalaVersion := "2.9.1"

crossScalaVersions ++= Seq("2.8.1", "2.9.0")

testOptions in Test in ThisBuild ++= (
  System.getProperty("sbinary.tests") match {
    case null   => Nil
    case num    => Seq(Tests.Argument("-s", num))
})
