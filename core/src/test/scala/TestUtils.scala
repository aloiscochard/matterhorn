package matterhorn

import scala.concurrent.ExecutionContext

import IO._

object TestUtils {
  val getLine = captureIO(readLine)
  val putStrLn = liftIO(println _)
  val threadDelay = liftIO(Thread.sleep _)
}
