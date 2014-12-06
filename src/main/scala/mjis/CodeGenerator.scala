package mjis

import java.io.{FileWriter, BufferedWriter}

import firm.{Util, Backend}

import scala.io.Source

class CodeGenerator(a: Unit) extends Phase[Unit] {
  def findings = List()
  def dumpResult(a: BufferedWriter) = {
    Source.fromFile("a.s").foreach(a.write(_))
  }
  def getResult(): Unit = {
    val asm = "a.s"
    Util.lowerSels()
    Backend.createAssembler(asm, "<input>")
    // concatenate our implementation of System_out_println to the assembly code
    val stdlib = Source.fromInputStream(this.getClass.getClassLoader.getResourceAsStream("System_out_println_64.s"))
    val fw = new BufferedWriter(new FileWriter(asm, true))
    stdlib.foreach(fw.write(_))
    fw.flush()
    Runtime.getRuntime.exec(s"gcc -m64 $asm").waitFor()
  }

}
