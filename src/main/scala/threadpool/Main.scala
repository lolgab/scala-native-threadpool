package threadpool

import scala.scalanative.libc.stdio
import scala.scalanative.unsafe._
import scala.scalanative.posix.pthread._
import scala.scalanative.libc.stdlib
import scala.scalanative.posix.sys.types._

object Main {
  val pthreadRoutine = new CFuncPtr1[Ptr[Byte], Ptr[Byte]] {
    def apply(ptr: Ptr[Byte]): Ptr[Byte] = {
      stdio.puts(c"Hello from thread!")
      null
    }
  }
  def main(args: Array[String]): Unit = {
    Pool.run(() => stdio.puts(c"Hello from thread"))
    Pool.run(() => stdio.puts(c"Hello from 2"))
    Pool.run(() => stdio.puts(c"sdcsd"))
    Pool.run(() => stdio.puts(c"HellosdcaD from thread"))
    Thread.sleep(1000)
    Pool.terminate()
    // scalanative.runtime.ExecutionContext.global.execute(new Runnable {
    //   def run(): Unit = {
    //     Pool.terminate()
    //   }
    // })
  }
}