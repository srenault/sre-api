val projectName = "sre-api"

val ScalaVersion = "2.13.8"
val Fs2Version = "2.5.10"
val Http4sVersion = "0.21.31"
val LogbackVersion = "1.2.10"
val CirceVersion = "0.13.0"
val CirceConfigVersion = "0.8.0"
val AnormVersion = "2.6.8"
val SqliteJdbcVersion = "3.32.3.3"
val Ofx4jVersion = "1.28"
val JsoupVersion = "1.12.2"
val ScalaCacheVersion = "0.28.0"
val ScalaCacheCatsVersion = "0.28.0"
val AwsSdkVersion = "1.11.615"
val ScalaTestVersion = "3.2.11"
val ScalaMockVersion = "4.4.0"
val CatsEffectTestScalaTestVersion = "0.4.2"
val EasyMockVersion = "4.2"
val JavaWebSocketVersion = "1.5.1"

val gitVersion = {
  import scala.sys.process._
  val GitShortVersion = """^.*([0-9abcdef]{7})$""".r
  Option("git describe --abbrev=7 --always" !!).map(_.trim.toLowerCase).map {
    case GitShortVersion(gitVersion) => gitVersion
    case other                       => other
  }.filterNot(_.isEmpty).getOrElse("x-SNAPSHOT")
}

scalacOptions ++= Seq("-Xlint")
logBuffered in Test := false

lazy val server = (project in file("."))
  .settings(
    organization := "sre",
    name := projectName,
    version := "0.0.1-SNAPSHOT",
    scalaVersion := ScalaVersion,
    libraryDependencies ++= Seq(
      "co.fs2"                    %% "fs2-core"                       % Fs2Version,
      "co.fs2"                    %% "fs2-io"                         % Fs2Version,
      "org.http4s"                %% "http4s-dsl"                     % Http4sVersion,
      "org.http4s"                %% "http4s-blaze-server"            % Http4sVersion,
      "org.http4s"                %% "http4s-blaze-client"            % Http4sVersion,
      "org.http4s"                %% "http4s-circe"                   % Http4sVersion,
      "org.http4s"                %% "http4s-scala-xml"               % Http4sVersion,
      "ch.qos.logback"            %  "logback-classic"                % LogbackVersion,
      "io.circe"                  %% "circe-parser"                   % CirceVersion,
      "io.circe"                  %% "circe-generic"                  % CirceVersion,
      "io.circe"                  %% "circe-literal"                  % CirceVersion,
      "io.circe"                  %% "circe-config"                   % CirceConfigVersion,
      "org.xerial"                % "sqlite-jdbc"                     % SqliteJdbcVersion,
      "org.playframework.anorm"   %% "anorm"                          % AnormVersion,
      "com.webcohesion.ofx4j"     % "ofx4j"                           % Ofx4jVersion,
      "com.github.cb372"          %% "scalacache-guava"               % ScalaCacheVersion,
      "com.github.cb372"          %% "scalacache-cats-effect"         % ScalaCacheCatsVersion,
      "com.amazonaws"             % "aws-java-sdk"                    % AwsSdkVersion,
      "org.java-websocket"        % "Java-WebSocket"                  % JavaWebSocketVersion,
      "org.scalatest"             %% "scalatest"                      % ScalaTestVersion % "test",
      "org.easymock"              % "easymock"                        % EasyMockVersion % "test",
      "com.codecommit"            %% "cats-effect-testing-scalatest"  % CatsEffectTestScalaTestVersion % "test"
    ),
    addCompilerPlugin("org.typelevel" %% "kind-projector"     % "0.10.3"),
    addCompilerPlugin("com.olegpy"     %% "better-monadic-for" % "0.3.1")
  ).
  settings(
    mainClass in assembly := Some("sre.api.Server"),
    assemblyMergeStrategy in assembly := {
      case PathList("META-INF", xs @ _*) => MergeStrategy.discard
      case x => MergeStrategy.first
    },
    assemblyJarName in assembly := s"$projectName.jar",
  )

scalacOptions ++= Seq("-Xlint")

val Http4sVersionNext = "0.23.12"
val CirceVersionNext = "0.14.3"
val FeralVersion = "0.1.0-M13"
val CatsEffectTestScalaTestVersionNext = "1.4.0"
val NatchezVersion = "0.1.6"
val NatchezHttp4s = "0.3.2"
val Log4catsVersion = "2.4.0"
val DeclineVersion = "2.3.0"

lazy val commons = (project in file("commons"))
  .settings(
    organization := "sre",
    name := "commons",
    version := "0.0.1-SNAPSHOT",
    scalaVersion := ScalaVersion,
    libraryDependencies ++= Seq(
      "io.circe"                  %% "circe-parser"                  % CirceVersion,
      "io.circe"                  %% "circe-generic"                 % CirceVersionNext,
      "org.http4s"                %% "http4s-dsl"                    % Http4sVersionNext,
      "org.typelevel"             %% "cats-effect"                   % "3.3.5",
      "software.amazon.awssdk"    % "s3"                             % "2.17.268",
      "org.typelevel"             %% "log4cats-core"                 % Log4catsVersion,
      "org.typelevel"             %% "log4cats-slf4j"                % Log4catsVersion
    )
  )

lazy val heatersProject = (project in file("heaters-api"))
  .settings(
    organization := "sre",
    name := "heaters-api",
    version := "0.0.1-SNAPSHOT",
    scalaVersion := ScalaVersion,
    libraryDependencies ++= Seq(
      "org.typelevel"             %% "feral-lambda"                  % FeralVersion,
      "org.typelevel"             %% "feral-lambda-http4s"           % FeralVersion,
      "org.http4s"                %% "http4s-scala-xml"              % Http4sVersionNext,
      "org.http4s"                %% "http4s-dsl"                    % Http4sVersionNext,
      "org.http4s"                %% "http4s-circe"                  % Http4sVersionNext,
      "org.http4s"                %% "http4s-blaze-client"           % Http4sVersionNext,
      "io.circe"                  %% "circe-parser"                  % CirceVersionNext,
      "io.circe"                  %% "circe-generic"                 % CirceVersionNext,
      "io.circe"                  %% "circe-literal"                 % CirceVersionNext,
      "ch.qos.logback"            %  "logback-classic"               % LogbackVersion,
      "org.tpolecat"              %% "natchez-xray"                  % NatchezVersion,
      "org.tpolecat"              %% "natchez-http4s"                % NatchezHttp4s
    )
  ).settings(
    assemblyJarName in assembly := s"heaters-api.jar",
    assemblyMergeStrategy in assembly := {
      case PathList("META-INF", xs @ _*) => MergeStrategy.discard
      case x => MergeStrategy.first
    },
  ).dependsOn(commons)

lazy val financeProject = (project in file("finance-api"))
  .settings(
    organization := "sre",
    name := "finance-api",
    version := "0.0.1-SNAPSHOT",
    scalaVersion := ScalaVersion,
    libraryDependencies ++= Seq(
      "org.typelevel"             %% "feral-lambda"                  % FeralVersion,
      "org.typelevel"             %% "feral-lambda-http4s"           % FeralVersion,
      "org.http4s"                %% "http4s-scala-xml"              % Http4sVersionNext,
      "org.http4s"                %% "http4s-dsl"                    % Http4sVersionNext,
      "org.http4s"                %% "http4s-circe"                  % Http4sVersionNext,
      "org.http4s"                %% "http4s-blaze-client"           % Http4sVersionNext,
      "io.circe"                  %% "circe-parser"                  % CirceVersionNext,
      "io.circe"                  %% "circe-generic"                 % CirceVersionNext,
      "io.circe"                  %% "circe-literal"                 % CirceVersionNext,
      "ch.qos.logback"            %  "logback-classic"               % LogbackVersion,
      "org.playframework.anorm"   %% "anorm"                         % AnormVersion,
      "org.jsoup"                 % "jsoup"                          % JsoupVersion,
      "com.webcohesion.ofx4j"     % "ofx4j"                          % Ofx4jVersion,
      "org.tpolecat"              %% "natchez-xray"                  % NatchezVersion,
      "org.tpolecat"              %% "natchez-http4s"                % NatchezHttp4s,
      "org.xerial"                % "sqlite-jdbc"                    % SqliteJdbcVersion,
      "org.typelevel"             %% "log4cats-core"                 % Log4catsVersion,
      "org.typelevel"             %% "log4cats-slf4j"                % Log4catsVersion,
      "org.scalatest"             %% "scalatest"                     % ScalaTestVersion % "test",
      "org.typelevel"             %% "cats-effect-testing-scalatest" % CatsEffectTestScalaTestVersionNext % "test"
    ),
    addCompilerPlugin("com.olegpy"     %% "better-monadic-for" % "0.3.1")
  ).settings(
    assemblyJarName in assembly := s"finance-api.jar",
    assemblyMergeStrategy in assembly := {
      case PathList("META-INF", xs @ _*) => MergeStrategy.discard
      case x => MergeStrategy.first
    },
  ).dependsOn(commons)

lazy val cliProject = (project in file("cli"))
  .settings(
    organization := "sre",
    name := "cli",
    version := "0.0.1-SNAPSHOT",
    scalaVersion := ScalaVersion,
    libraryDependencies ++= Seq(
      "com.monovore" %% "decline"              % DeclineVersion,
      "com.monovore" %% "decline-effect"       % DeclineVersion,
      "io.circe"     %% "circe-parser"         % CirceVersion,
      "io.circe"     %% "circe-config"         % CirceConfigVersion,
      "org.http4s"   %% "http4s-blaze-client"  % Http4sVersionNext,
    )
  ).settings(
    assemblyJarName in assembly := s"cli.jar"
  ).dependsOn(financeProject, heatersProject)
