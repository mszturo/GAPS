name := "GAPS"

version := "0.1"

scalaVersion := "2.12.8"

lazy val testLibs = Seq(
  "org.scalatest" % "scalatest_2.10" % "2.0"
).map(_ % Test)

lazy val typesafe = Seq(
  "com.typesafe" % "config" % "1.3.4"
)

libraryDependencies ++=
  typesafe ++
  testLibs