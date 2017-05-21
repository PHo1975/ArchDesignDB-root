package util

import scala.collection.mutable.ArrayBuffer


/**
  * Created by Peter on 01.02.2015.
  */

object Log {

  type LogListener = (String) => Unit

  protected val logListeners: ArrayBuffer[LogListener] = ArrayBuffer[LogListener]()

  def addLogListener(l: LogListener): Unit = logListeners += l

  def w(st: String): Unit = {
    val elem = Thread.currentThread().getStackTrace()(2)
    notifyLogListeners(elem.getClassName + " " + elem.getMethodName + " " + elem.getFileName + "(" + elem.getLineNumber + "): " + st)
  }

  def e(st: String): Unit = {
    val elem = Thread.currentThread().getStackTrace()(2)
    notifyLogListeners(elem.getClassName + " " + elem.getMethodName + " " + elem.getFileName + "(" + elem.getLineNumber + "): " + st)
  }

  def e(t: Throwable): Unit = {
    val elem = Thread.currentThread().getStackTrace()(2)
    notifyLogListeners(elem.getClassName + " " + elem.getMethodName + " " + elem.getFileName + "(" + elem.getLineNumber + "): " + t ++ "\n" + t.getStackTrace.mkString("\n   "))
  }

  protected def notifyLogListeners(st: String): Unit = {
    for (l <- logListeners) l(st)
    println(st)
  }

  def e(st: String, t: Throwable): Unit = {
    val elem = Thread.currentThread().getStackTrace()(2)
    notifyLogListeners(elem.getClassName + " " + elem.getMethodName + " " + elem.getFileName + "(" + elem.getLineNumber + "): " + st + " " + t + "\n" + t.getStackTrace.mkString("\n   "))
  }

  def e(st: String, ar: Array[StackTraceElement]): Unit = {
    val elem = ar(0)
    notifyLogListeners(elem.getClassName + " " + elem.getMethodName + " " + elem.getFileName + "(" + elem.getLineNumber + "): " + st + "\n " + ar.mkString("\n  "))
  }
}

