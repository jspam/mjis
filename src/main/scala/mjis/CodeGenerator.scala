package mjis

import java.io.BufferedWriter

import firm.{Dump, Program, Util, Backend}

import scala.io.Source
import scala.collection.JavaConversions._

class CodeGenerator(a: Unit) extends Phase[Unit] {
  def findings = List()
  def dumpResult(a: BufferedWriter) = {
    Source.fromFile("a.s").foreach(a.write(_))
  }
  def getResult(): Unit = {
    // I'd prefer to do this in dumpResult, but libFirm segfaults when
    // you try to dump a graph after it has generated assembler code for it
    Program.getGraphs.foreach(Dump.dumpGraph(_, "-input"))
    Util.lowerSels()
    Backend.createAssembler("a.s", "input")
    Runtime.getRuntime.exec("gcc -m32 a.s lib/System_out_println_32.s")
  }

}
