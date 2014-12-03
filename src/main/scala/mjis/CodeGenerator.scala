package mjis

import java.io.BufferedWriter

import firm.{Program, Util, Backend}
import mjis.util.FirmDumpHelper

import scala.io.Source
import scala.collection.JavaConversions._

class CodeGenerator(a: Unit) extends Phase[Unit] {
  def findings = List()
  def dumpResult(a: BufferedWriter) = {
    Source.fromFile("a.s").foreach(a.write(_))
  }
  def getResult(): Unit = {
    Util.lowerSels()
    Backend.createAssembler("a.s", "<input>")
    Runtime.getRuntime.exec("gcc -m64 a.s lib/System_out_println_64.s").waitFor()
  }

}
