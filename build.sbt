val Fs2Version = "2.4.0"
val http4sVersion = "1.0.0-M3"

lazy val root = (project in file("."))
  .settings(
    organization := "com.zorgonout",
    name := "MuseOSCReceiver",
    version := "0.0.1-SNAPSHOT",
    scalaVersion := "2.13.3",
    libraryDependencies ++= Seq(
      "co.fs2" %% "fs2-core" % Fs2Version,
      "co.fs2" %% "fs2-io" % Fs2Version,
      "org.scodec" %% "scodec-core" % "1.11.7",
      "org.scodec" %% "scodec-stream" % "2.0.0",
      "org.http4s" %% "http4s-dsl" % http4sVersion,
      "org.http4s" %% "http4s-blaze-server" % http4sVersion,
      "org.http4s" %% "http4s-blaze-client" % http4sVersion,
      "ch.qos.logback" % "logback-classic" % "1.2.3",
      "org.scalatest" %% "scalatest" % "3.2.0" % "test"
    ),
    //    addCompilerPlugin("org.typelevel" %% "kind-projector"     % "0.10.3"),
    //    addCompilerPlugin("com.olegpy"    %% "better-monadic-for" % "0.3.1")
  )

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-language:higherKinds",
  "-language:postfixOps",
  "-feature",
  "-Xfatal-warnings",
)
