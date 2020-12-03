name := "cpg bug poc"
scalaVersion in ThisBuild := "2.13.3"
organization := "de.tubs"
version := "1.0-SNAPSHOT"
licenses += "APACHE-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")
enablePlugins(GitVersioning)

val cpgVersion = "1.2.28"

lazy val dependencies =
  new {
    val scopt                   = "com.github.scopt"       %% "scopt"                      % "3.7.1"

    val codepropertygraph       = "io.shiftleft"           %% "codepropertygraph"          % cpgVersion
    val codepropertygraphProtos = "io.shiftleft"           %% "codepropertygraph-protos"   % cpgVersion
    val semanticcpg             = "io.shiftleft"           %% "semanticcpg"                % cpgVersion

    val logbackClassic          = "ch.qos.logback"         %  "logback-classic"            % "1.2.3"
    val betterFiles             = "com.github.pathikrit"   %% "better-files"               % "3.8.0"
    val scalaParallelCollection = "org.scala-lang.modules" %% "scala-parallel-collections" % "0.2.0"
    val scalatest               = "org.scalatest"          %% "scalatest"                  % "3.1.0" % Test
    val fastparse               = "com.lihaoyi"            %% "fastparse"                  % "2.2.2"
    val scalaParserCombinators  = "org.scala-lang.modules" %% "scala-parser-combinators"   % "1.1.2"
    val commonsio               = "commons-io"              % "commons-io"                  % "2.8.0"
  }


libraryDependencies ++= Seq(
  dependencies.scopt,
  dependencies.codepropertygraph,
  dependencies.codepropertygraphProtos,
  dependencies.semanticcpg,
  dependencies.logbackClassic,
  dependencies.betterFiles,
  dependencies.scalaParallelCollection,
  dependencies.scalatest,
  dependencies.commonsio
)


ThisBuild / resolvers ++= Seq(
    Resolver.mavenLocal,
    Resolver.bintrayRepo("shiftleft", "maven"),
    Resolver.bintrayRepo("mpollmeier", "maven"),
      "Artifactory release local" at "https://shiftleft.jfrog.io/shiftleft/libs-release-local",
      "Apache public" at "https://repository.apache.org/content/groups/public/",
      "Sonatype OSS" at "https://oss.sonatype.org/content/repositories/public",
      "Bedatadriven for SOOT dependencies" at "https://nexus.bedatadriven.com/content/groups/public"
  )

scalacOptions ++= Seq(
  "-deprecation",                      // Emit warning and location for usages of deprecated APIs.
  "-encoding", "utf-8",                // Specify character encoding used by source files.
  "-explaintypes",                     // Explain type errors in more detail.
  "-feature",                          // Emit warning and location for usages of features that should be imported explicitly.
  "-language:existentials",            // Existential types (besides wildcard types) can be written and inferred
  "-language:experimental.macros",     // Allow macro definition (besides implementation and application)
  "-language:higherKinds",             // Allow higher-kinded types
  "-language:implicitConversions",     // Allow definition of implicit functions called views
  "-unchecked",                        // Enable additional warnings where generated code depends on assumptions.
  "-Xcheckinit",                       // Wrap field accessors to throw an exception on uninitialized access.
  // "-Xfatal-warnings",                  // Fail the compilation if there are any warnings.
  "-Xlint:adapted-args",               // Warn if an argument list is modified to match the receiver.
  "-Xlint:constant",                   // Evaluation of a constant arithmetic expression results in an error.
  "-Xlint:delayedinit-select",         // Selecting member of DelayedInit.
  "-Xlint:doc-detached",               // A Scaladoc comment appears to be detached from its element.
  "-Xlint:inaccessible",               // Warn about inaccessible types in method signatures.
  "-Xlint:infer-any",                  // Warn when a type argument is inferred to be `Any`.
  "-Xlint:missing-interpolator",       // A string literal appears to be missing an interpolator id.
  "-Xlint:option-implicit",            // Option.apply used implicit view.
  "-Xlint:package-object-classes",     // Class or object defined in package object.
  "-Xlint:poly-implicit-overload",     // Parameterized overloaded implicit methods are not visible as view bounds.
  "-Xlint:private-shadow",             // A private field (or class parameter) shadows a superclass field.
  "-Xlint:stars-align",                // Pattern sequence wildcard must align with sequence component.
  "-Xlint:type-parameter-shadow",      // A local type parameter shadows a type already in scope.
  "-Ywarn-dead-code",                  // Warn when dead code is identified.
  "-Ywarn-extra-implicit",             // Warn when more than one implicit parameter section is defined.
  //"-Xlint:nullary-override",           // Warn when non-nullary def f() overrides nullary def f.
  "-Xlint:nullary-unit",               // Warn when nullary methods return Unit.
  "-Ywarn-numeric-widen",              // Warn when numerics are widened.
  "-Ywarn-unused:implicits",           // Warn if an implicit parameter is unused.
  "-Ywarn-unused:imports",             // Warn if an import selector is not referenced.
  "-Ywarn-unused:locals",              // Warn if a local definition is unused.
  "-Ywarn-unused:params",              // Warn if a value parameter is unused.
  "-Ywarn-unused:patvars",             // Warn if a variable bound in a pattern is unused.
  "-Ywarn-unused:privates",            // Warn if a private member is unused.
  // "-Ywarn-value-discard"               // Warn when non-Unit expression results are unused.
)

compile / javacOptions ++= Seq("-Xlint:all", "-Xlint:-cast", "-g")
Test / fork :=  false
testOptions += Tests.Argument(TestFrameworks.JUnit, "-a", "-v")

checkstyleConfigLocation := CheckstyleConfigLocation.File("config/checkstyle/google_checks.xml")
checkstyleSeverityLevel := Some(CheckstyleSeverityLevel.Info)

enablePlugins(JavaAppPackaging)
