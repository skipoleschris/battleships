import sbt._
import Keys._

object BuildSettings {
  val buildOrganization = "com.equalexperts.battleships"
  val buildScalaVersion = "2.10.0"
  val buildVersion      = "0.1"

  val buildSettings = Defaults.defaultSettings ++
                      Seq (organization  := buildOrganization,
                           scalaVersion  := buildScalaVersion,
                           version       := buildVersion,
                           scalacOptions ++= Seq("-deprecation", "-language:all"))
}

object Dependencies {

  val extraResolvers = Seq("Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
                           "Sonatype" at "http://oss.sonatype.org/content/repositories/snapshots/")

  // //////////////////////////////////////////////////////////////////////////////////////////
  // Compile Libraries

  val unfilteredVersion = "0.6.7"
  val unfilteredLibrary = "net.databinder" %% "unfiltered" % unfilteredVersion % "compile"
  val unfilteredJson = "net.databinder" %% "unfiltered-json" % unfilteredVersion % "compile"
  val unfilteredNetty = "net.databinder" %% "unfiltered-netty" % unfilteredVersion % "compile"
  val unfilteredNettyServer = "net.databinder" %% "unfiltered-netty-server" % unfilteredVersion % "compile"
  val unfiltered = Seq(unfilteredLibrary, unfilteredJson, unfilteredNetty, unfilteredNettyServer)

  val jodaTime = "joda-time" % "joda-time" % "1.6.2" % "compile"
  val utilities = Seq(jodaTime)

  val scalaz = "org.scalaz" %% "scalaz-core" % "7.0.0-M7" % "compile"
  val functional = Seq(scalaz)

  // //////////////////////////////////////////////////////////////////////////////////////////
  // Testing Libraries

  val specs2 = "org.specs2" %% "specs2" % "1.13" % "test"
  val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.10.0" % "compile"
  val testing = Seq(specs2, scalaCheck)

  // Dependency groups
  val coreDeps = unfiltered ++ utilities ++ functional ++ testing
}

object BattleshipsServerBuild extends Build {
  import Dependencies._
  import BuildSettings._

  lazy val battleshipsServerProject = Project ("battleships-server", file ("."),
           settings = buildSettings ++ Seq(resolvers ++= extraResolvers, libraryDependencies ++= coreDeps))
}
