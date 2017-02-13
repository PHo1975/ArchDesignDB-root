package util

//import java.util.logging.{ Level, Logger, ConsoleHandler}

/**
 * Created by Kathi on 01.02.2015.
 */

object Log {

   /*lazy val logger={
     System.setProperty( "java.util.logging.SimpleFormatter.format","%4$s: %5$s %6$s\n")
     val res= Logger.getLogger("DB")
     res.setLevel(Level.INFO)
     val consoleHandler=new ConsoleHandler(){
       setOutputStream(System.out)
     }
     res.addHandler(consoleHandler)
     res
   }*/

   def w(st:String)={
     val elem=Thread.currentThread().getStackTrace()(2)
     println(elem.getClassName+" "+elem.getMethodName+" "+elem.getFileName+"("+elem.getLineNumber+"): "+st)
   }

  def e(st:String)={
    val elem=Thread.currentThread().getStackTrace()(2)
    println(elem.getClassName+" "+elem.getMethodName+" "+elem.getFileName+"("+elem.getLineNumber+"): "+st)
  }

  def e(t:Throwable)={
    val elem=Thread.currentThread().getStackTrace()(2)
    println(elem.getClassName+" "+elem.getMethodName+" "+elem.getFileName+"("+elem.getLineNumber+"): "+t++"\n"+t.getStackTrace.mkString("\n   "))
  }

  def e(st:String,t:Throwable)={
    val elem=Thread.currentThread().getStackTrace()(2)
    println(elem.getClassName+" "+elem.getMethodName+" "+elem.getFileName+"("+elem.getLineNumber+"): "+st+" "+t+"\n"+t.getStackTrace.mkString("\n   "))
  }

  def e(st:String,ar:Array[StackTraceElement])={
    val elem=ar(0)
    println(elem.getClassName+" "+elem.getMethodName+" "+elem.getFileName+"("+elem.getLineNumber+"): "+st+"\n "+ar.mkString("\n  "))
  }
}

