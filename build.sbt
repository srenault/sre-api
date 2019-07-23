val projectName = "sre-api"

val Http4sVersion = "0.20.4"
val Specs2Version = "4.6.0"
val LogbackVersion = "1.2.3"
val CirceVersion = "0.11.1"
val CirceConfigVersion = "0.6.1"
val AnormVersion = "2.6.2"
val SqliteJdbcVersion = "3.28.0"
val Ofx4jVersion = "1.17"
val JsoupVersion = "1.12.1"
val CronVersion =  "0.1.0"
val ScalaCacheVersion = "0.28.0"
val ScalaCacheCatsVersion = "0.28.0"
val AwsSdkVersion = "1.11.596"

val gitVersion = {
  import scala.sys.process._
  val GitShortVersion = """^.*([0-9abcdef]{7})$""".r
  Option("git describe --abbrev=7 --always" !!).map(_.trim.toLowerCase).map {
    case GitShortVersion(gitVersion) => gitVersion
    case other                       => other
  }.filterNot(_.isEmpty).getOrElse("x-SNAPSHOT")
}

lazy val root = (project in file("."))
  .settings(
    organization := "sre",
    name := projectName,
    version := "0.0.1-SNAPSHOT",
    scalaVersion := "2.12.6",
    libraryDependencies ++= Seq(
      "org.http4s"                %% "http4s-dsl"             % Http4sVersion,
      "org.http4s"                %% "http4s-blaze-server"    % Http4sVersion,
      "org.http4s"                %% "http4s-blaze-client"    % Http4sVersion,
      "org.http4s"                %% "http4s-circe"           % Http4sVersion,
      "org.specs2"                %% "specs2-core"            % Specs2Version % "test",
      "ch.qos.logback"            %  "logback-classic"        % LogbackVersion,
      "io.circe"                  %% "circe-parser"           % CirceVersion,
      "io.circe"                  %% "circe-generic"          % CirceVersion,
      "io.circe"                  %% "circe-literal"          % CirceVersion,
      "io.circe"                  %% "circe-config"           % CirceConfigVersion,
      "org.xerial"                % "sqlite-jdbc"             % SqliteJdbcVersion,
      "org.playframework.anorm"   %% "anorm"                  % AnormVersion,
      "com.webcohesion.ofx4j"     % "ofx4j"                   % Ofx4jVersion,
      "org.jsoup"                 % "jsoup"                   % JsoupVersion,
      "eu.timepit"                %% "fs2-cron-core"          % CronVersion,
      "com.github.cb372"          %% "scalacache-guava"       % ScalaCacheVersion,
      "com.github.cb372"          %% "scalacache-cats-effect" % ScalaCacheCatsVersion,
      "com.amazonaws"             % "aws-java-sdk"            % AwsSdkVersion
    ),
    addCompilerPlugin("org.spire-math" %% "kind-projector"     % "0.9.10"),
    addCompilerPlugin("com.olegpy"     %% "better-monadic-for" % "0.3.0")
  ).
  settings(
    mainClass in assembly := Some("sre.api.Server"),
    assemblyMergeStrategy in assembly := {
      case PathList("META-INF", xs @ _*) => MergeStrategy.discard
      case x => MergeStrategy.first
    },
    assemblyJarName in assembly := s"$projectName.jar",
  )

scalacOptions ++= Seq(
  "-Xlint"
)
