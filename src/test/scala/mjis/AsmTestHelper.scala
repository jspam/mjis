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

  private def stripComments: String => String = "\\s*#.*".r.replaceAllIn(_, "")

  private def normalizeLabels(code: String): String = {
    val labelMappings = createLabelMappings(code)
    ".L(\\d+)".r.replaceAllIn(code, match_ => ".L" + labelMappings(match_.group(1)))
  }

  def isIsomorphic(left: String, right: String): MatchResult =
    BeMatcher(be(normalizeLabels(stripComments(right)))).apply(normalizeLabels(stripComments(left)))
}
