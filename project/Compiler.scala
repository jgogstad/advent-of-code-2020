package jgogstad.sbt

import sbt.{Def, _}
import sbt.Keys._
import Versions.V

object Compiler extends AutoPlugin {
  override def trigger = allRequirements

  override def buildSettings: Seq[Def.Setting[_]] = Seq(
    scalaVersion := V.build.Scala213Version,
    /*
    Always fork JVMs launched from Compile configuration. This is typically processes launched with runMain. We always
    want them to run in a separate JVM so that it shuts down properly. If not forked, thread pools are not shut down
    and CPU usage goes up.
    */
    Compile / fork := true,
    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % V.build.MonadicFor),
    addCompilerPlugin(("org.typelevel" % "kind-projector" % V.build.KindProjector).cross(CrossVersion.full)),
    scalacOptions ++= Seq(
      "-Ymacro-annotations",    // Enable annotation macros
      "-target:11"              // Target JRE 11
    )
  )
}

