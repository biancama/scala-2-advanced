val scala3Version = "3.2.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "scala-2-advanced",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
               "org.scalameta" %% "munit" % "0.7.29" % Test)
  )
