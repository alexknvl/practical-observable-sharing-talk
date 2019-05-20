autoCompilerPlugins := true

lazy val common = Seq(
  organization      := "com.alexknvl",
  version           := "0.0.1-SNAPSHOT",
  scalaVersion      := "2.12.6",
  resolvers ++= Seq(
    Resolver.sonatypeRepo("public"),
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots"),
    Resolver.mavenLocal,
    "bintray-djspiewak-maven" at "https://dl.bintray.com/djspiewak/maven"),
  scalacOptions ++= List(
    "-deprecation",
    "-encoding", "UTF-8",
    "-explaintypes",
    "-Yrangepos",
    "-feature",
    "-Xfuture",
    "-Ypartial-unification",
    "-language:higherKinds",
    "-language:existentials",
    "-language:implicitConversions",
    "-language:experimental.macros",
    "-unchecked",
    "-Yno-adapted-args",
    "-opt-warnings",
    "-Xlint:_,-type-parameter-shadow",
    "-Xsource:2.13",
    "-Ywarn-dead-code",
     "-Ywarn-extra-implicit",
    "-Ywarn-inaccessible",
    "-Ywarn-infer-any",
    "-Ywarn-nullary-override",
    "-Ywarn-nullary-unit",
    "-Ywarn-numeric-widen",
     "-Ywarn-unused:_,-imports",
    "-Ywarn-value-discard",
     "-opt:l:inline",
     "-opt-inline-from:<source>",
//    "-Yno-predef"
  ),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4"),
  libraryDependencies += "com.lihaoyi" %% "acyclic" % "0.1.7" % "provided",
  addCompilerPlugin("com.lihaoyi" %% "acyclic" % "0.1.7"),
  scalacOptions += "-P:acyclic:force",
  libraryDependencies ++= List(
    compilerPlugin("org.scalaz" %% "scalaz-plugin"   % "0.0.6" cross CrossVersion.full),
    "org.scalaz" %% "scalaz-plugin-library" % "0.0.6"
  )
)

val ParsebackVersion = "0.3"
val AttoVersion = "0.6.3"
val ScalaParserCombinatorsVersion = "1.1.1"

lazy val libBase = project.in(file("libBase"))
  .settings(name := "parsing-base")
  .settings(common:_*)
  .settings(libraryDependencies ++= List(
    "org.typelevel"  %% "cats-core"      % "1.1.0",
    "org.typelevel"  %% "cats-effect"    % "0.10",
    "com.codecommit" %% "parseback-core" % ParsebackVersion,
    "com.codecommit" %% "parseback-cats" % ParsebackVersion,
    "org.tpolecat"   %% "atto-core"      % AttoVersion,
    "org.scala-lang.modules" %% "scala-parser-combinators" % ScalaParserCombinatorsVersion))

lazy val appTest = project.in(file("appTest"))
  .settings(name := "parsing-test")
  .settings(common:_*)
  .dependsOn(libBase)

lazy val root = project.in(file("."))
  .settings(publishArtifact := false)
  .settings(common : _*)
  .aggregate(libBase, appTest)
