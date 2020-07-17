name := "httptest"

version := "0.1"

scalaVersion := "2.13.3"

libraryDependencies ++= Seq(
    "org.asynchttpclient" % "async-http-client" % "2.12.1",
    "com.lihaoyi" %% "ujson" % "1.1.0",
    "ch.qos.logback" % "logback-classic" % "1.3.0-alpha5"
)
