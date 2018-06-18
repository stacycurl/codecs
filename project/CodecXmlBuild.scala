import sbt.Keys._
import sbt._
//import scoverage.ScoverageKeys._


object CodecXmlBuild extends Build {
  val (twoEleven, twoTwelve) = ("2.11.8", "2.12.3")

  lazy val codecXml = (project in file(".")
    settings(
      organization              := "com.github.stacycurl",
      scalaVersion              := twoTwelve,
      crossScalaVersions        := Seq(twoTwelve, twoEleven),
      scalacOptions             := Seq("-feature", "-Xfatal-warnings", "-deprecation", "-unchecked", "-target:jvm-1.8"),
      javacOptions              := Seq("-source", "1.8", "-target", "1.8", "-Xlint"),
      maxErrors                 := 1,
      parallelExecution in Test := true,
      resolvers += "Stacy Curl's repo" at "http://dl.bintray.com/stacycurl/repo/",
      resolvers += "jcenter" at "http://jcenter.bintray.com",
      libraryDependencies <++= scalaVersion(dependencies(twoTwelve → List(
        "org.scala-lang.modules"     %% "scala-xml"        % "1.1.0",
        "org.scala-lang"             % "scala-compiler"    % twoTwelve,
        "org.scala-lang"             % "scala-library"     % twoTwelve    % "test",
        "com.github.stacycurl"       %% "delta-core"       % "1.1.2"     % "test",
        "com.github.stacycurl"       %% "delta-matchers"   % "1.1.0"     % "test"
      ), twoEleven → List(
        "com.github.stacycurl"       %% "delta-matchers"   % "1.0.19" % "test"
      ))),
      initialize := {
        val _ = initialize.value
        require(sys.props("java.specification.version") == "1.8", "Java 8 is required for this project.")
      }
    )
//    settings(Publishing.settings: _*)
    settings addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")
  )

  private def dependencies(modules: (String, List[ModuleID])*)(version: String) = modules.toMap.apply(version)
}
