androidBuild

scalacOptions := Seq(
  "-feature",
  "-language:implicitConversions",
  "-deprecation",
  "-unchecked"
)
javacOptions in Compile ++= "-source" :: "1.7" :: "-target" :: "1.7" :: Nil

name := "s-m2"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  "jcenter" at "http://jcenter.bintray.com"
)

libraryDependencies ++= Seq(
  aar("org.macroid" %% "macroid" % "2.0.0-20150427"),
  "org.scodec" %% "scodec-bits" % "1.1.0",
  "org.scodec" %% "scodec-core" % "1.10.0",
  "commons-io" % "commons-io" % "2.5",
  "org.scalactic" %% "scalactic" % "2.2.6",
  "org.scalatest" %% "scalatest" % "2.2.6" % "test"
)
libraryDependencies ++= {
  if (scalaBinaryVersion.value startsWith "2.10")
    Seq(compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full))
  else Nil
}

//debugIncludesTests in Android := true
//libraryDependencies ++= Seq(
//    "com.android.support.test" % "runner" % "0.2",
//    "com.android.support.test.espresso" % "espresso-core" % "2.1")
//instrumentTestRunner in Android :=
//  "android.support.test.runner.AndroidJUnitRunner"

testOptions in Test += Tests.Argument("-oF")

proguardScala in Android := true