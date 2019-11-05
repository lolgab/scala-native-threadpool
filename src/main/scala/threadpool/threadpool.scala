package threadpool
import scala.scalanative.libc.stdlib._
import scala.scalanative.posix.pthread._
import scala.scalanative.posix.sys.types._
import scala.scalanative.unsafe._
import scala.collection.mutable
import scala.scalanative.libc.stdio
import scala.util.control.NonFatal
import scala.util.Properties

@extern
private[threadpool] object CApi {
  def pthread_create(
      thread: Ptr[pthread_t],
      attr: Ptr[pthread_attr_t],
      startroutine: CFuncPtr1[Ptr[Byte], Ptr[Byte]],
      args: Ptr[Byte]
  ): CInt = extern
}

object Pool {
  val size = 4

  private var terminating: Boolean = false

  private val finished = new Array[Boolean](size)

  private val taskList = mutable.ListBuffer.empty[() => Unit]

  private val mutex = {
    val res = malloc(pthread_mutex_t_size).asInstanceOf[Ptr[pthread_mutex_t]]
    if (pthread_mutex_init(res, null) != 0)
      throw new Exception("Failed to initialize mutex.")
    res
  }

  private def createCondition() = {
    val res = malloc(pthread_cond_t_size).asInstanceOf[Ptr[pthread_cond_t]]
    if(pthread_cond_init(res, null) != 0)
      throw new Exception("Failed to initialize condition.")
    res
  }

  private val emptyCondition = createCondition()

  private val finishedCondition = createCondition()

  private val pthreadRoutine = new CFuncPtr1[Ptr[Byte], Ptr[Byte]] {
    @inline private def runningLoop(): Unit = {
      if(acquiring(!terminating)) {
        while(taskList.isEmpty && !terminating) {
          pthread_cond_wait(emptyCondition, mutex)
        }
        val taskToRun = if(taskList.nonEmpty) taskList.remove(0) else null
        pthread_mutex_unlock(mutex)
        if (taskToRun != null) taskToRun()
        runningLoop()
      } else pthread_mutex_unlock(mutex)
    }

    @inline private def terminateLoop(): Unit = {
      if(acquiring(taskList.nonEmpty)) {
        val taskToRun = taskList.remove(0)
        pthread_mutex_unlock(mutex)
        taskToRun()
        terminateLoop()
      } else pthread_mutex_unlock(mutex)
    }

    def apply(ptr: Ptr[Byte]): Ptr[Byte] = {
      runningLoop()
      stdio.puts(c"runningLoop()")
      terminateLoop()
      stdio.puts(c"terminateLoop()")
      synch (finished(!ptr) = true)
      stdio.puts(c"synch (finished(ptr.toLong.toInt) = true)")
      pthread_cond_broadcast(finishedCondition)
      stdio.puts(c"pthread_cond_broadcast(finishedCondition)")
      null
    }
  }

  val threads = {
    val attr = malloc(pthread_attr_t_size).asInstanceOf[Ptr[pthread_attr_t]]
    if(pthread_attr_init(attr) != 0)
      throw new Exception("Failed to initialize thread attr.")
    pthread_attr_setdetachstate(attr, PTHREAD_CREATE_DETACHED)
    for (i <- 0 until size) yield {
      val pthreadPtr = malloc(sizeof[Ptr[Byte]]).asInstanceOf[Ptr[pthread_t]]
      val dataPtr = malloc(1)
      !dataPtr = i.toByte
      CApi.pthread_create(pthreadPtr, attr, pthreadRoutine, dataPtr)
      pthreadPtr
    }
  }

  @specialized(Boolean)
  def synch[T](f: => T): T = {
    pthread_mutex_lock(mutex)
    val res = f
    pthread_mutex_unlock(mutex)
    res
  }

  @specialized(Boolean)
  def acquiring[T](f: => T): T = {
    pthread_mutex_lock(mutex)
    f
  }

  def run(f: () => Unit): Unit = {
    synch {
      taskList += f
      pthread_cond_signal(emptyCondition)
    }
  }

  def terminate(): Unit = {
    synch {
      terminating = true
      while(finished.exists(_ == false)) {
        pthread_cond_broadcast(emptyCondition)
        pthread_cond_wait(finishedCondition, mutex)
        println(s"""terminating = $terminating
                    |finished(0) = ${finished(0)}
                    |finished(1) = ${finished(1)}
                    |finished(2) = ${finished(2)}
                    |finished(3) = ${finished(3)}""".stripMargin)
      }
    }
  }
}
