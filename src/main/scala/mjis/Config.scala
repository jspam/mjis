package mjis

import java.nio.file.{Paths, Path}

case class Config(stopAfter: String = "",
                  firmDump: Boolean = false,
                  file: Option[Path] = null,
                  useFirmBackend: Boolean = false,
                  printTimings: Boolean = false,
                  verbose: Boolean = false,
                  inlining: Boolean = true,
                  outFile: Path = Paths.get("a.out").toAbsolutePath) {
  def asmOutFile: Path = Paths.get(outFile.toString + ".s")
}
