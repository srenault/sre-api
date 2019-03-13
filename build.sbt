val Http4sVersion = "0.20.0-M6"
val Specs2Version = "4.1.0"
val LogbackVersion = "1.2.3"
val CirceVersion = "0.11.1"
val CirceConfigVersion = "0.5.0"
val AnormVersion = "2.6.2"
val SqliteJdbcVersion = "3.23.1"
val ScalaXmlVersion = "1.1.1"
val Ofx4jVersion = "1.14"
val JsoupVersion = "1.11.3"
val CronVersion =  "0.0.12"
val ScalaCacheVersion = "0.27.0"
val ScalaCacheCatsVersion = "0.27.0"

lazy val root = (project in file("."))
  .settings(
    organization := "sre",
    name := "dashboard",
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
      "org.scala-lang.modules"    %% "scala-xml"              % ScalaXmlVersion,
      "com.webcohesion.ofx4j"     % "ofx4j"                   % Ofx4jVersion,
      "org.jsoup"                 % "jsoup"                   % JsoupVersion,
      "eu.timepit"                %% "fs2-cron-core"          % CronVersion,
      "com.github.cb372"          %% "scalacache-guava"       % ScalaCacheVersion,
      "com.github.cb372"          %% "scalacache-cats-effect" % ScalaCacheCatsVersion
    ),
    addCompilerPlugin("org.spire-math" %% "kind-projector"     % "0.9.6"),
    addCompilerPlugin("com.olegpy"     %% "better-monadic-for" % "0.2.4")
  )

scalacOptions ++= Seq(
  "-Xlint"
)
