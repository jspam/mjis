package mjis

import org.scalatest.matchers.{BeMatcher, MatchResult}
import org.scalatest.words.MatcherWords._
import System.{lineSeparator => n}

import scala.collection.mutable

object AsmTestHelper {
  def getBlockName: String => Option[String] = "^\\.(\\w+):".r.findFirstMatchIn(_).map(m => m.group(1))

  private def getBlockLineNumbers(code: Seq[String]): mutable.Map[String, Int] = {
    val result = mutable.Map[String, Int]()
    code.zipWithIndex.foreach {
      case (lineContents, lineNo) =>
        getBlockName(lineContents) match {
          case Some(label) => result(label) = lineNo
          case None =>
        }
    }

    result
  }

  def isIsomorphic(left: String, right: String): MatchResult = {
    def stripComments: String => String = "\\s*#.*".r.replaceAllIn(_, "")
    val labelMapping = mutable.HashMap[String, String]("" -> "")
    val labelMappingSources = mutable.HashMap[String, String]("" -> "")

    val (leftLines, rightLines) = ("[\r\n]+".r.split(left), "[\r\n]+".r.split(right))
    val rightBlockLineNumbers = getBlockLineNumbers(rightLines)

    var rightLineNo = -1
    leftLines.zipWithIndex.foreach {
      case (leftLineOriginal, leftLineNo) =>
        rightLineNo += 1

        val leftLine = stripComments(leftLineOriginal)
        val rightLine = stripComments(rightLines(rightLineNo))
        def failure(msg: String): MatchResult = MatchResult(matches = false, msg
          + s":${n}Left (line $leftLineNo): $leftLine$n"
          + s"Right (line $rightLineNo): $rightLine", "")

        val leftBlockName = getBlockName(leftLine)
        val rightBlockName = getBlockName(rightLine)

        (leftBlockName, rightBlockName) match {
          case (Some(leftLabel), Some(rightLabel)) =>
            // Both sides contain a label
            labelMapping.get(leftLabel) match {
              case Some(mappedRightLabel) =>
                rightLineNo = rightBlockLineNumbers(mappedRightLabel)
              case None =>
                labelMapping(leftLabel) = rightLabel
                labelMappingSources(leftLabel) = s"left: $leftLine (line $leftLineNo), right: $rightLine (line $rightLineNo)"
            }

          case (Some(_), None) =>
            return failure(s"Got label on left, got no label on right")
          case (None, Some(_)) =>
            return failure(s"Got no label on left, got label on right")

          case (None, None) =>
            // Both sides contain an instruction
            def getLabels: String => Seq[String] = "\\.(\\w+)".r.findAllMatchIn(_).map(m => m.group(1)).toSeq
            def stripLabels: String => String = "\\.\\w+".r.replaceAllIn(_, ".XXX")
            val (leftWithoutLabels, rightWithoutLabels) = (stripLabels(leftLine), stripLabels(rightLine))

            val leftEqualsRight = BeMatcher(be(rightWithoutLabels)).apply(leftWithoutLabels)
            if (!leftEqualsRight.matches) return MatchResult(matches = false,
              s"Error while comparing lines $leftLineNo/$rightLineNo:$n: ${leftEqualsRight.failureMessage}", "")

            val (leftLabels, rightLabels) = (getLabels(leftLine), getLabels(rightLine))
            assert(leftLabels.length == rightLabels.length)
            leftLabels zip rightLabels foreach {
              case (leftLabel, rightLabel) => labelMapping.get(leftLabel) match {
                case Some(expectedRightLabel) =>
                  if (rightLabel != expectedRightLabel)
                    return failure(s"Got right label $rightLabel, expected $expectedRightLabel$n"
                      + s"(Got the label mapping from ${labelMappingSources(leftLabel)})")
                case None =>
                  labelMapping(leftLabel) = rightLabel
                  labelMappingSources(leftLabel) = s"left: $leftLine (line $leftLineNo), right: $rightLine (line $rightLineNo)"
              }
            }
        }
    }

    MatchResult(matches = true, "", "")
  }
}
