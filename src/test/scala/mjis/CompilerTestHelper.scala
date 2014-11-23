package mjis

import java.io.StringReader
import System.{ lineSeparator => n }
import mjis.ast._
import org.scalatest._
import scala.reflect._

object CompilerTestHelper {
  def fromMethod(method: String, mainMethod: Boolean = true): String = s"class Test {$n$method$n" +
    (if (mainMethod) s"public static void main(String[] args){}}" else "}")
  def fromStatements(statements: String, mainMethod: Boolean = true): String =
    fromMethod(s"public void test() {$n$statements$n}", mainMethod)

  def assertExec[P <: Phase[_]: ClassTag](input: String): P = Compiler.exec(new StringReader(input)) match {
    case Left(phase) => phase
    case Right(findings) => Assertions.fail(s"Compilation up to ${classTag[P].runtimeClass.getName} failed. Findings:$n${findings.mkString(n)}")
  }

  def assertExecClass[P <: Phase[Program]: ClassTag](cls: String): ClassDecl = assertExec[P](cls).result.classes(0)
  def assertExecMethod[P <: Phase[Program]: ClassTag](method: String): MethodDecl = assertExecClass[P](fromMethod(method)).methods(0)
  def assertExecStatements[P <: Phase[Program]: ClassTag](statements: String): List[Statement] = assertExecClass[P](fromStatements(statements)).methods(0).body.statements

  def assertExecFailure[P <: Phase[_]: ClassTag](input: String): List[Finding] = Compiler.exec(new StringReader(input)) match {
    case Left(phase) => Assertions.fail(s"Compilation up to ${classTag[P].runtimeClass.getName} succeeded, expected it to fail.")
    case Right(findings) => findings
  }
  def assertExecFailureWith[F <: Finding: ClassTag, P <: Phase[_]: ClassTag](input: String) = {
    val findings = assertExecFailure[P](input)
    if (!findings.head.isInstanceOf[F]) {
      Assertions.fail(s"Wrong type of finding, expected a ${classTag[F].runtimeClass.getName}, " +
        s"got a ${findings.head.getClass}: ${findings.head}")
    }
  }
}
