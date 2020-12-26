import jgogstad.sbt.Versions.V

ThisBuild / organization := "jgogstad"

lazy val `advent` = (project in file("."))
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel"          %% "cats-effect"              % V.fp.CatsEffect,
      "co.fs2"                 %% "fs2-core"                 % V.fp.Fs2Core,
      "co.fs2"                 %% "fs2-io"                   % V.fp.Fs2Core,
      "org.typelevel"          %% "cats-core"                % V.fp.Cats,
      "org.typelevel"          %% "squants"                  % V.types.Squants,
      "com.github.valskalla"   %% "odin-core"                % V.logging.Odin,
      "org.scala-lang.modules" %% "scala-collection-contrib" % V.types.ScalaCollectionContrib,
      "org.typelevel"          %% "spire"                    % V.math.Spire,
      "org.jgrapht"            % "jgrapht-core"              % V.math.Jgrapht,
      "com.lihaoyi"            %% "fastparse"                % V.parsing.Fastparse,
      "org.scalanlp"           %% "breeze"                   % V.math.Breeze
    ),
    scalacOptions -= "-Xfatal-warnings"
  )
