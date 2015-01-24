package mjis.util

import java.io.File

import firm.{Dump, Graph}

object FirmDumpHelper {

  def dumpGraph(graph: Graph, suffix: String) = {
    new File("./firm-dumps").mkdir() // will silently fail if the directory already exists
    firm.bindings.binding_irdump.ir_set_dump_path("./firm-dumps")
    Dump.addDumpFlags(firm.bindings.binding_irdump.ir_dump_flags_t.ir_dump_flag_idx_label.`val`)
    Dump.dumpGraph(graph, suffix)
  }

}
