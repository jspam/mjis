package mjis

import java.nio.file.{Path, Paths, Files}

case class Config(stopAfter: String = "", file: Option[Path] = null, useFirmBackend: Boolean = false)
