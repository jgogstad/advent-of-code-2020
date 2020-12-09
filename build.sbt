import jgogstad.sbt.Versions.V

ThisBuild / organization := "jgogstad"

lazy val `advent` = (project in file("."))
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel"          %% "cats-effect"              % V.fp.CatsEffect,
      "io.chrisdavenport"      %% "cats-effect-time"         % V.fp.CatsEffectTime,
      "io.chrisdavenport"      %% "cats-time"                % V.fp.CatsTime,
      "com.github.cb372"       %% "cats-retry"               % V.fp.CatsRetry,
      "co.fs2"                 %% "fs2-core"                 % V.fp.Fs2Core,
      "co.fs2"                 %% "fs2-io"                   % V.fp.Fs2Core,
      "org.typelevel"          %% "cats-core"                % V.fp.Cats,
      "eu.timepit"             %% "refined-cats"             % V.types.Refined,
      "io.estatico"            %% "newtype"                  % V.types.NewType,
      "com.beachape"           %% "enumeratum"               % V.types.Enumeratum,
      "com.beachape"           %% "enumeratum-cats"          % V.types.Enumeratum,
      "org.typelevel"          %% "squants"                  % V.types.Squants,
      "com.github.valskalla"   %% "odin-core"                % V.logging.Odin,
      "org.scala-lang.modules" %% "scala-collection-contrib" % V.types.ScalaCollectionContrib
    )
  )
