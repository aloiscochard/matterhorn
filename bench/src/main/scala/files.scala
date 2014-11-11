package matterhorn
package bench

import scala.concurrent.ExecutionContext

import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Mode}

import java.io.BufferedWriter
import java.nio.charset.Charset
import java.nio.file.{Files, Path}

import TestUtils._

import IO._
import RTS.defaultRTS.unsafePerformIO_

@BenchmarkMode(Array(Mode.SingleShotTime))
class FilesBenchmark {

  def data: Array[Char] = Array.range(0, dataSize).map(_.toChar)

  def dataSize = 1
  def size = 1024 * 1024 * 8

  def writeBytes(size: Int): Unit = {
    @scala.annotation.tailrec
    def f(i: Int)(bw: BufferedWriter): Unit = if (i != 0) { bw.write(data, 0, dataSize); f(i -1)(bw) }

    val fp = Files.createTempFile("foo", "bar")
    println(fp)
    val bw = Files.newBufferedWriter(fp, Charset.forName("UTF-8"))
    f(size)(bw)
    bw.close
  }

  @Benchmark
  def main = writeBytes(size)

  def createTempFile(prefix: String, suffix: String): IO[Path] = captureIO(Files.createTempFile(prefix, suffix))
  def newBufferedWriter(path: Path): IO[BufferedWriter] = captureIO(Files.newBufferedWriter(path, Charset.forName("UTF-8")))
  def write(bw: BufferedWriter): IO[Unit] = captureIO(bw.write(data, 0, dataSize))
  def close(bw: BufferedWriter): IO[Unit] = captureIO(bw.close)

  def writeBytesIO(size: Int): IO[Unit] = {
    def f(i: Int)(bw: BufferedWriter): IO[Unit] = if (i == 0) unitIO else write(bw) *> f(i - 1)(bw)

    for {
      fp  <- createTempFile("foo", "bar")
      _   <- putStrLn(fp)
      os  <- newBufferedWriter(fp)
      _   <- f(size)(os) *> close(os)
    } yield ()
  }

  @Benchmark
  def mainIO = unsafePerformIO_(writeBytesIO(size))
}

object FilesProfiling extends App {
  val mainIO =
    putStrLn("Press <enter> to start") *> getLine *>
    (new FilesBenchmark).writeBytesIO(1024 * 1024 * 100)
}
