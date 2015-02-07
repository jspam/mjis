package mjis

import java.nio.file.{Paths, Path}

case class Config(stopAfter: String = "",
                  firmDump: Boolean = false,
                  file: Option[Path] = null,
                  eval: Boolean = false,
                  useFirmBackend: Boolean = false,
                  printTimings: Boolean = false,
                  verbose: Boolean = false,
                  inlining: Boolean = true,
                  optimizeUnreachableGraphs: Boolean = false,
                  outFile: Path = Paths.get("a.out").toAbsolutePath) {
  def asmOutFile: Path = Paths.get(outFile.toString + ".s")
}
