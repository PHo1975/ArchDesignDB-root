package util

import scala.collection.mutable.ArrayBuffer


/**
  * Created by Peter on 01.02.2015.
  */

object Log {

  type LogListener = (String,Boolean) => Unit

  protected val logListeners: ArrayBuffer[LogListener] = ArrayBuffer[LogListener]()

  def addLogListener(l: LogListener): Unit = logListeners += l

  def i(st:String):Unit = notifyLogListeners(st,error = false)

  def w(st: String): Unit = {
    val elem = Thread.currentThread().getStackTrace()(2)
    notifyLogListeners(elem.getClassName + " " + elem.getMethodName + " " + elem.getFileName + "(" + elem.getLineNumber + "): " + st,error = false)
  }

  def e(st: String): Unit = {
    val elem = Thread.currentThread().getStackTrace()(2)
    notifyLogListeners(elem.getClassName + " " + elem.getMethodName + " " + elem.getFileName + "(" + elem.getLineNumber + "): " + st,error = true)
  }

  def e_withoutTrace(st:String): Unit = {
    notifyLogListeners(st,error = true)
  }

  def e(t: Throwable): Unit = {
    val elem = Thread.currentThread().getStackTrace()(2)
    notifyLogListeners(elem.getClassName + " " + elem.getMethodName + " " + elem.getFileName + "(" + elem.getLineNumber + "): " + t ++ "\n" + t.getStackTrace.mkString("\n   "),error = true)
  }

  def e(st: String, t: Throwable): Unit = {
    val elem = Thread.currentThread().getStackTrace()(2)
    notifyLogListeners(elem.getClassName + " " + elem.getMethodName + " " + elem.getFileName + "(" + elem.getLineNumber + "): " + st + " " + t + "\n" + t.getStackTrace.mkString("\n   "),error = true)
  }

  def e(st: String, ar: Array[StackTraceElement]): Unit = {
    val elem = ar(0)
    notifyLogListeners(elem.getClassName + " " + elem.getMethodName + " " + elem.getFileName + "(" + elem.getLineNumber + "): " + st + "\n " + ar.mkString("\n  "),error = true)
  }

  protected def notifyLogListeners(st: String,error:Boolean): Unit = {
    for (l <- logListeners) l(st,error)
    println(st)
  }

}

