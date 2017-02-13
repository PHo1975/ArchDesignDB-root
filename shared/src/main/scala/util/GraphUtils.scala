package util

import definition.expression.VectorConstant

object GraphUtils {
   def createRotator(center:VectorConstant,angle:Double):VectorConstant=>VectorConstant= {
    val cosa=math.cos(angle)
    val sina=math.sin(angle)
    (v:VectorConstant)=> {
      val x=v.x-center.x
      val y=v.y-center.y
      new VectorConstant(x*cosa-y*sina+center.x,x*sina+y*cosa+center.y,0)
    }    
  }
}