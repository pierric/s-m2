androidBuildAar

scalaVersion := "2.11.8"

scalacOptions := Seq(
  "-feature",
  "-language:implicitConversions",
  "-deprecation",
  "-unchecked"
)
javacOptions in Compile ++= "-source" :: "1.7" :: "-target" :: "1.7" :: Nil

name := "s-m2"
organization := "org.recursive"
version := "0.1-SNAPSHOT"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  "jcenter" at "http://jcenter.bintray.com"
)

libraryDependencies ++= Seq(
  "org.scodec" %% "scodec-bits" % "1.1.0",
  "org.scodec" %% "scodec-core" % "1.10.0",
  "commons-io" % "commons-io" % "2.5",
  "org.scala-lang.modules" %% "scala-xml" % "1.0.5",
  "org.scalactic" %% "scalactic" % "2.2.6",
  "org.scalatest" %% "scalatest" % "2.2.6" % "test"
)
libraryDependencies ++= {
  if (scalaBinaryVersion.value startsWith "2.10")
    Seq(compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full))
  else Nil
}

publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath+"/.m2/repository")))

proguardScala in Android := true