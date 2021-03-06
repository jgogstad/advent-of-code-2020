package jgogstad.sbt

object Versions {
  val V = this

  object build {
    val Scala213Version = "2.13.3"
    val MonadicFor      = "0.3.1"
    val KindProjector   = "0.11.0"
  }

  object fp {
    val Cats           = "2.2.0"
    val CatsEffect     = "2.3.0"
    val Fs2Core        = "2.4.4"
  }

  object logging {
    val Odin = "0.9.1"
  }

  object math {
    val Spire   = "0.17.0"
    val Jgrapht = "1.5.0"
    val Breeze = "1.1"
  }

  object types {
    val ScalaCollectionContrib = "0.2.2"
    val Squants                = "1.6.0"
  }

  object parsing {
    val Fastparse = "2.2.2"
  }

}
