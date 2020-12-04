package jgogstad.utils

import java.io.FileNotFoundException
import java.net.URI
import java.nio.file.Paths

import cats.effect.{Blocker, ContextShift, IO, Resource, Sync}
import cats.instances.string._
import cats.syntax.all._
import cats.ApplicativeError

import scala.io.Source
import fs2.io.file.readAll
import squants.information.Information
import squants.information.Mebibytes
import fs2.Stream

object FileIo {

  /**
   * Reads contents of file relative to classpath root.
   *
   * Inspect the `resourceDirectories` sbt key for sources,
   * e.g. `sbt 'show mymodule / Test / resourceDirectories'`
   */
  def contentsOf[F[_]: Sync: ContextShift](file: String, blocker: Blocker, chunkSize: Information = Mebibytes(2)): fs2.Stream[F, String] = {
    val resolveFile = ApplicativeError[F, Throwable].catchNonFatal(getClass.getClassLoader.getResource(file)).map(Option.apply)
    Stream.eval(resolveFile).flatMap {
      case Some(url) =>
        fs2.io.file.readAll(Paths.get(url.toURI), blocker, chunkSize.toBytes.toInt)
          .through(fs2.text.utf8Decode)
          .through(fs2.text.lines)
      case None => fs2.Stream.raiseError(new FileNotFoundException(file))
    }

//    Option(getClass.getClassLoader.getResource(file)).map(_.toURI) match {
//      case Some(url) => {
//        Resource
//          .make(Sync[F].delay(Source.fromURI(url)))(s => Sync[F].delay(s.close()))
//          .use { source =>
//            Sync[F].delay(source.getLines().mkString("\n"))
//          }
//      }
//      case None => (new FileNotFoundException(file)).raiseError[F, String]
//    }
  }
}
