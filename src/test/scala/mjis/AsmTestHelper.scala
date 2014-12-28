package mjis

import org.scalatest.matchers.{BeMatcher, MatchResult}
import org.scalatest.words.MatcherWords._
import System.{lineSeparator => n}

import scala.collection.mutable

object AsmTestHelper {
  private def createLabelMappings(code: String): Map[String, Int] = {
    val result = mutable.Map[String, Int]()
    var lastLabelIdx = 0
    "(?m)^.L(\\d+)".r.findAllMatchIn(code).foreach { match_ =>
      assert(!result.contains(match_.group(1)))
      result(match_.group(1)) = lastLabelIdx
      lastLabelIdx += 1
    }
    result.toMap
  }

  private def createRegisterMappings(code: String): Map[String, Int] = {
    val result = mutable.Map[String, Int]()
    var lastRegIdx = 0
    "%REG(\\d+)".r.findAllMatchIn(code).foreach { match_ =>
      if (!result.contains(match_.group(1))) {
        result(match_.group(1)) = lastRegIdx
        lastRegIdx += 1
      }
    }
    result.toMap
  }

  private def stripComments: String => String = "\\s*#.*".r.replaceAllIn(_, "")

  private def normalize(code: String): String = {
    val labelMappings = createLabelMappings(code)
    val tmp = ".L(\\d+)".r.replaceAllIn(code, match_ => ".L" + labelMappings(match_.group(1)))

    val registerMappings = createRegisterMappings(code)
    "%REG(\\d+)".r.replaceAllIn(tmp, match_ => "%REG" + registerMappings(match_.group(1)))
  }

  def isIsomorphic(left: String, right: String): MatchResult =
    BeMatcher(be(normalize(stripComments(right)))).apply(normalize(stripComments(left)))
}
