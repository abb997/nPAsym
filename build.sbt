name := "nPAsymm"

version := "0.1"

scalaVersion := "2.12.7"

lazy val root = (project in file(".")).
  settings (
     cancelable in Global := true
    ,libraryDependencies ++= {
      Seq(
         "org.scalactic" %% "scalactic" % "3.0.8"
        ,"org.scalatest" %% "scalatest" % "3.0.8" % "test"
      )}
  )

logBuffered in Test := false
assemblyOption in assembly := (assemblyOption in assembly).value.copy(includeScala = true, includeDependency = true)

mainClass in Compile := Some("nPAsymm_")
