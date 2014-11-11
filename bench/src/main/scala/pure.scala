package matterhorn
package bench

import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Mode}

import IO._
import RTS.defaultRTS.{unsafePerformIO_ => run}

@Fork(1)
@BenchmarkMode(Array(Mode.Throughput))
class PureBenchmark {

  def maps(size: Int): Unit = {
    def f(io: IO[Unit])(n: Int): IO[Unit] = if (n == 0) io else f(io.map(_ => ()))(n - 1)
    run(f(unitIO)(size))
  }

  def aps(size: Int): Unit = {
    def f(n: Int): IO[Unit] = if (n == 0) unitIO else unitIO *> f(n - 1)
    run(f(size))
  }

  def mapsandaps(size: Int): Unit = {
    def f(n: Int): IO[Unit] = if (n == 0) unitIO.map(_ => ()) else unitIO.map(_ => ()) *> f(n - 1)
    run(f(size))
  }

  @Benchmark def maps64 = maps(64)
  @Benchmark def aps64 = maps(64)
  @Benchmark def mapsandaps64 = maps(64)

  @Benchmark def maps1024 = maps(1024)
  @Benchmark def aps1024 = maps(1024)
  @Benchmark def mapsandaps1024 = maps(1024)
}
