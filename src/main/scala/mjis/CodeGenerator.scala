package mjis

import java.io.BufferedWriter

import firm.{Util, Backend}

import scala.io.Source

class CodeGenerator(a: Unit) extends Phase[Unit] {
  def findings = List()
  def dumpResult(a: BufferedWriter) = Source.fromFile("a.s").foreach(a.write(_))
  def getResult(): Unit = {
    Util.lowerSels()
    Backend.createAssembler("a.s", "input")
    Runtime.getRuntime.exec("gcc -m32 a.s")
  }

}
