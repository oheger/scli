/*
 * Copyright 2020 The Developers Team.
 *
 * Licensed under the Apache License, Version 2.0 (the "License")
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.github.scli

import java.io.IOException
import java.nio.file.{Path, Paths}

import com.github.scli.ParameterExtractor._
import com.github.scli.ParametersTestHelper._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.SortedSet
import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

object CliExtractorOpsSpec {
  /** Key for the option containing multiple numeric values. */
  private val KeyNumbers = "--numbers"

  /** Key for an option that yields a single numeric value. */
  private val KeyAnswer = "--answer"

  /** Key for an option that yields a single boolean value. */
  private val KeyFlag = "--flag"

  /** Key for an option containing a single path value. */
  private val KeyPath = "--path"

  /** A key for an option that does not exist. */
  private val UndefinedKey = "--non-existing-option"

  /** The numeric values for ''KeyNumbers''. */
  private val NumberValues = List(0, 8, 15)

  /** The number value of the ''answer'' option. */
  private val NumberValue = 42

  /** The value of the option with a path value. */
  private val PathValue = Paths get "test"

  /** A map with test parameters used by the tests. */
  private val TestParameters: Parameters = Map(KeyNumbers -> NumberValues.map(_.toString),
    KeyAnswer -> List(NumberValue.toString),
    KeyFlag -> List("true"),
    KeyPath -> List(PathValue.toString))

  /**
   * Executes the given extractor on the test parameters.
   *
   * @param ext        the extractor
   * @param parameters the parameters
   * @tparam A the result type of the extractor
   * @return the result produced by the extractor
   */
  private def runExtractor[A](ext: CliExtractor[A], parameters: Parameters = TestParameters): A =
    ParameterExtractor.runExtractor(ext, parameters)(DefaultConsoleReader)._1

  /**
   * Creates a [[ParamModel]] object from the given components.
   *
   * @param triedNumbers the numbers component
   * @param triedAnswer  the answer component
   * @param triedFlag    the flag component
   * @param triedPath    the path component
   * @return a ''Try'' with the resulting model
   */
  private def createModel(triedNumbers: Try[Iterable[Int]], triedAnswer: Try[Int], triedFlag: Try[Boolean],
                          triedPath: Try[Option[Path]]): Try[ParamModel] =
    createRepresentation(triedNumbers, triedAnswer, triedFlag, triedPath) { (nums, a, f, p) =>
      ParamModel(nums.toList, a, f, p)
    }

  /**
   * Returns an extractor that extracts a [[ParamModel]] object.
   *
   * @return the extractor
   */
  private def paramModelExtractor(): CliExtractor[Try[ParamModel]] =
    for {
      extNumbers <- multiOptionValue(KeyNumbers).toInt
      extAnswer <- multiOptionValue(KeyAnswer).toInt.single.mandatory
      extFlag <- multiOptionValue(KeyFlag).toBoolean.single.mandatory
      extPath <- multiOptionValue(KeyPath).toPath.single
    } yield createModel(extNumbers, extAnswer, extFlag, extPath)

  /**
   * Creates a test component for a given index.
   *
   * @param index the index
   * @return the test component with this index
   */
  private def createComponent(index: Int): String = s"testValue$index"

  /**
   * Creates a successful ''Try'' with the test component with the given
   * index.
   *
   * @param index the index
   * @return the ''Success'' with this test component
   */
  private def triedComponent(index: Int): Try[String] =
    Success(createComponent(index))

  /**
   * Creates a sequence with test components that can be used to test the
   * creation of a representation.
   *
   * @param count the number of components
   * @return the sequence with the test components
   */
  private def createComponents(count: Int): IndexedSeq[String] =
    (1 to count) map createComponent

  /**
   * A data class that combines the test option values.
   *
   * @param numbers value for the list of numbers
   * @param answer  value for the single number
   * @param flag    value for the flag
   * @param path    value for the path
   */
  case class ParamModel(numbers: List[Int],
                        answer: Int,
                        flag: Boolean,
                        path: Option[Path])

}

/**
 * Test class for the DSL to define complex ''CliExtractor'' objects.
 */
class CliExtractorOpsSpec extends AnyFlatSpec with Matchers {

  import CliExtractorOpsSpec._

  "ParameterExtractor" should "extract multiple string values" in {
    val strValues = NumberValues.map(_.toString)
    val ext = multiOptionValue(KeyNumbers)

    val result = runExtractor(ext)
    result should be(Success(strValues))
  }

  it should "extract a single string value" in {
    val ext = optionValue(KeyAnswer)

    val result = runExtractor(ext)
    result should be(Success(Some(NumberValue.toString)))
  }

  it should "report a failure for a single value option that has multiple values" in {
    val ext = optionValue(KeyNumbers)

    val result = runExtractor(ext)
    result match {
      case Failure(exception: ParameterExtractionException) =>
        val failure = exception.failures.head
        failure.key.key should be(KeyNumbers)
        failure.message should include("should have a single value")
      case r => fail("Unexpected result: " + r)
    }
  }

  it should "extract numeric values" in {
    val ext = multiOptionValue(KeyNumbers).toInt

    val result = runExtractor(ext)
    result should be(Success(NumberValues))
  }

  it should "extract a single numeric value" in {
    val ext = optionValue(KeyAnswer).toInt

    val result = runExtractor(ext)
    result should be(Success(Some(NumberValue)))
  }

  it should "extract a single mandatory numeric value" in {
    val ext = optionValue(KeyAnswer).toInt.mandatory

    val result = runExtractor(ext)
    result should be(Success(NumberValue))
  }

  it should "extract a single flag value" in {
    val ext = optionValue(KeyFlag).toBoolean.mandatory

    val result = runExtractor(ext)
    result should be(Success(true))
  }

  it should "extract multiple flag values" in {
    val ext = multiOptionValue(KeyFlag).toBoolean

    val result = runExtractor(ext)
    result.get should contain only true
  }

  it should "extract a single path value" in {
    val ext = optionValue(KeyPath).toPath.mandatory

    val result = runExtractor(ext)
    result should be(Success(PathValue))
  }

  it should "extract multiple path values" in {
    val ext = multiOptionValue(KeyPath).toPath

    val result = runExtractor(ext)
    result.get should contain only PathValue
  }

  it should "convert multiple string values to lower case" in {
    val OrgValues = List("TEST", "test", "Test", "TesT")
    val Key = "multiStringOption"
    val parameters = TestParameters.copy(parametersMap = TestParameters.parametersMap + (pk(Key) -> OrgValues))
    val ext = multiOptionValue(Key).toLower

    val result = runExtractor(ext, parameters).get
    result should have size OrgValues.size
    result.toSet should contain only "test"
  }

  it should "convert a string value to lower case" in {
    val Key = "stringOption"
    val parameters = TestParameters.copy(parametersMap = TestParameters.parametersMap + (pk(Key) -> List("TesT")))
    val ext = optionValue(Key).toLower.mandatory

    val result = runExtractor(ext, parameters)
    result should be(Success("test"))
  }

  it should "convert multiple string values to upper case" in {
    val OrgValues = List("TEST", "test", "Test", "TesT")
    val Key = "multiStringOption"
    val parameters = TestParameters.copy(parametersMap = TestParameters.parametersMap + (pk(Key) -> OrgValues))
    val ext = multiOptionValue(Key).toUpper

    val result = runExtractor(ext, parameters).get
    result should have size OrgValues.size
    result.toSet should contain only "TEST"
  }

  it should "convert a string value to upper case" in {
    val Key = "stringOption"
    val parameters = TestParameters.copy(parametersMap = TestParameters.parametersMap + (pk(Key) -> List("TesT")))
    val ext = optionValue(Key).toUpper.mandatory

    val result = runExtractor(ext, parameters)
    result should be(Success("TEST"))
  }

  it should "support mapping the values extracted by an extractor" in {
    val mapFunc: Int => Int = _ + 1
    val intExt = multiOptionValue(KeyNumbers).toInt
    val ext = intExt mapTo mapFunc
    val expectedValues = NumberValues map mapFunc

    val result = runExtractor(ext)
    result should be(Success(expectedValues))
  }

  it should "support mapping the single value extracted by an extractor" in {
    val ext = optionValue(KeyAnswer)
      .toInt
      .mapTo(_ + 1)
      .mandatory

    val result = runExtractor(ext)
    result should be(Success(NumberValue + 1))
  }

  it should "convert values to an enum" in {
    val EnumValues = List("larry", "curly", "moe")
    val mapping = NumberValues.map(_.toString).zip(EnumValues).toMap
    val ext = multiOptionValue(KeyNumbers)
      .toEnum(mapping.get)

    val result = runExtractor(ext)
    result should be(Success(EnumValues))
  }

  it should "convert a single value to an enum" in {
    val enumFunc: String => Option[Int] = s => Some(s.length)
    val ExpLength = PathValue.toString.length
    val ext = optionValue(KeyPath)
      .toEnum(enumFunc)

    val result = runExtractor(ext)
    result should be(Success(Some(ExpLength)))
  }

  it should "support checking whether a multi option is defined" in {
    val ext = multiOptionValue(KeyAnswer).isDefined

    val result = runExtractor(ext)
    result should be(Success(true))
  }

  it should "support checking whether a multi option is defined if it is not" in {
    val ext = multiOptionValue(UndefinedKey).isDefined

    val result = runExtractor(ext)
    result should be(Success(false))
  }

  it should "support checking whether an option is defined" in {
    val ext = optionValue(KeyAnswer).isDefined

    val result = runExtractor(ext)
    result should be(Success(true))
  }

  it should "support checking whether an option is defined if it is not" in {
    val ext = optionValue(UndefinedKey).isDefined

    val result = runExtractor(ext)
    result should be(Success(false))
  }

  it should "report a failure for an undefined mandatory option" in {
    val ext = multiOptionValue(UndefinedKey).single.mandatory

    val result = runExtractor(ext)
    result match {
      case Failure(exception: ParameterExtractionException) =>
        exception.failures.head.key.key should be(UndefinedKey)
      case r => fail("Unexpected result: " + r)
    }
  }

  it should "support checking for the multiplicity of an extractor" in {
    val ext = multiOptionValue(KeyNumbers).toInt.multiplicity(atMost = NumberValues.size)

    val result = runExtractor(ext)
    result should be(Success(NumberValues))
  }

  it should "handle an unbounded multiplicity for an extractor" in {
    val ext = multiOptionValue(KeyNumbers).toInt.multiplicity(atLeast = 1)

    val result = runExtractor(ext)
    result should be(Success(NumberValues))
  }

  it should "report a failure if not enough values are present" in {
    val ext = multiOptionValue(KeyAnswer).multiplicity(atLeast = 2)

    val result = runExtractor(ext)
    result match {
      case Failure(exception: ParameterExtractionException) =>
        val failure = exception.failures.head
        failure.key.key should be(KeyAnswer)
        failure.message should include("at least 2")
      case r => fail("Unexpected result: " + r)
    }
  }

  it should "report a failure if too many values are present" in {
    val ext = multiOptionValue(KeyNumbers).multiplicity(atMost = NumberValues.size - 1)

    val result = runExtractor(ext)
    result match {
      case Failure(exception: ParameterExtractionException) =>
        val failure = exception.failures.head
        failure.key.key should be(KeyNumbers)
        failure.message should include("at most " + (NumberValues.size - 1))
      case r => fail("Unexpected result: " + r)
    }
  }

  it should "support setting a fallback value for a multi option if the extractor yields a value" in {
    val ext = multiOptionValue(KeyAnswer).fallbackValues("fallback")
      .toInt.single.mandatory

    val result = runExtractor(ext)
    result should be(Success(NumberValue))
  }

  it should "support setting a fallback value for a multi option" in {
    val ext = multiOptionValue(UndefinedKey).fallback(constantOptionValue(NumberValue.toString))
      .toInt.single.mandatory

    val result = runExtractor(ext)
    result should be(Success(NumberValue))
  }

  it should "support setting fallback values for a multi option" in {
    val ext = multiOptionValue(UndefinedKey)
      .toInt
      .fallbackValues(NumberValues.head, NumberValues.tail: _*)

    val result = runExtractor(ext)
    result should be(Success(NumberValues))
  }

  it should "ignore fallback values in case of a failure" in {
    val ext = multiOptionValue(KeyPath)
      .toInt
      .fallbackValues(NumberValue)

    runExtractor(ext) match {
      case Failure(exception: ParameterExtractionException) =>
        exception.failures.head.key.key should be(KeyPath)
      case r => fail("Unexpected result: " + r)
    }
  }

  it should "support setting a fallback value of a single option if the extractor yields a value" in {
    val ext = optionValue(KeyAnswer).fallbackValue("not accessed")
      .toInt.mandatory

    val result = runExtractor(ext)
    result should be(Success(NumberValue))
  }

  it should "support setting a fallback extractor for a single option" in {
    val extFallback = constantOptionValue(NumberValue).single
    val ext = optionValue(UndefinedKey)
      .toInt.fallback(extFallback).mandatory

    val result = runExtractor(ext)
    result should be(Success(NumberValue))
  }

  it should "support setting a fallback value for a single option" in {
    val ext = optionValue(UndefinedKey).toInt
      .fallbackValueWithDesc(None, NumberValue).mandatory

    val result = runExtractor(ext)
    result should be(Success(NumberValue))
  }

  it should "support combining multiple options to a data object" in {
    val ExpModel = ParamModel(NumberValues, NumberValue, flag = true, Some(PathValue))
    val ext = paramModelExtractor()

    val result = runExtractor(ext)
    result should be(Success(ExpModel))
  }

  it should "handle failures when combining multiple options" in {
    def checkFailureContained(ex: ParameterExtractionException, keys: String*): Unit = {
      keys foreach { key =>
        ex.failures.find(_.key.key == key) should not be None
      }
    }

    val params: Parameters = Map(KeyNumbers -> ("noNumber" :: NumberValues.map(_.toString)),
      KeyAnswer -> List("xy"),
      KeyFlag -> List("undefined"))
    val ext = paramModelExtractor()

    val result = runExtractor(ext, params)
    result match {
      case Failure(exception: ParameterExtractionException) =>
        checkFailureContained(exception, KeyNumbers, KeyAnswer, KeyFlag)
        val context = exception.parameterContext
        context.parameters.parametersMap should be(params.parametersMap)
      case r => fail("Unexpected result: " + r)
    }
  }

  it should "create a representation from two components" in {
    val c1 = Success(NumberValue)
    val c2 = Success(PathValue)

    val triedValues = createRepresentation(c1, c2)(List(_, _))
    triedValues should be(Success(List(NumberValue, PathValue)))
  }

  it should "handle failures when creating a representation from components" in {
    val modelCtx = new ParameterModel.ModelContext(Map.empty, SortedSet.empty, ParameterModel.EmptyAliasMapping,
      None, Nil)
    val context = ParameterContext(TestParameters, modelCtx, DummyConsoleReader)
    val failure1 = ExtractionFailure(pk(KeyFlag), "Flag failure", context)
    val failure2 = ExtractionFailure(pk(KeyAnswer), "Answer failure", context)
    val exception1 = ParameterExtractionException(failure1)
    val exception2 = ParameterExtractionException(failure2)
    val c1 = Failure(exception1)
    val c2 = Failure(exception2)

    val triedValues = createRepresentation(c1, c2)((_, _) => throw new IllegalStateException("Failure"))
    triedValues match {
      case Failure(exception: ParameterExtractionException) =>
        exception.failures should be(List(failure1, failure2))
        exception.parameterContext should be(context)
      case r => fail("Unexpected result: " + r)
    }
  }

  it should "handle other exceptions when creating a representation from components" in {
    val exception1 = new IOException("failure 1")
    val exception2 = new IllegalStateException("failure 2")
    val c1 = Failure(exception1)
    val c2 = Failure(exception2)

    def checkFailure(failure: ExtractionFailure, exception: Throwable): Unit = {
      failure.key.key should be("")
      failure.message should be(exception.getMessage)
      failure.context.parameters should be(Parameters(Map.empty, Set.empty))
      val modelCtx = failure.context.modelContext
      modelCtx.options should have size 0
      modelCtx.inputs should have size 0
    }

    val triedValues = createRepresentation(c1, c2)((_, _) => throw new IllegalStateException("Failure"))
    triedValues match {
      case Failure(exception: ParameterExtractionException) =>
        exception.failures should have size 2
        checkFailure(exception.failures.head, exception1)
        checkFailure(exception.failures(1), exception2)
      case r => fail("Unexpected result: " + r)
    }
  }

  it should "create a representation from three components" in {
    val triedValues = createRepresentation(triedComponent(1), triedComponent(2),
      triedComponent(3))(List(_, _, _))

    triedValues.get should contain theSameElementsInOrderAs createComponents(3)
  }

  it should "create a representation from four components" in {
    val triedValues = createRepresentation(triedComponent(1), triedComponent(2),
      triedComponent(3), triedComponent(4))(List(_, _, _, _))

    triedValues.get should contain theSameElementsInOrderAs createComponents(4)
  }

  it should "create a representation from five components" in {
    val triedValues = createRepresentation(triedComponent(1), triedComponent(2),
      triedComponent(3), triedComponent(4), triedComponent(5))(List(_, _, _, _, _))

    triedValues.get should contain theSameElementsInOrderAs createComponents(5)
  }

  it should "create a representation from six components" in {
    val triedValues = createRepresentation(triedComponent(1), triedComponent(2),
      triedComponent(3), triedComponent(4), triedComponent(5),
      triedComponent(6))(List(_, _, _, _, _, _))

    triedValues.get should contain theSameElementsInOrderAs createComponents(6)
  }

  it should "create a representation from seven components" in {
    val triedValues = createRepresentation(triedComponent(1), triedComponent(2),
      triedComponent(3), triedComponent(4), triedComponent(5),
      triedComponent(6), triedComponent(7))(List(_, _, _, _, _, _, _))

    triedValues.get should contain theSameElementsInOrderAs createComponents(7)
  }

  it should "create a representation from eight components" in {
    val triedValues = createRepresentation(triedComponent(1), triedComponent(2),
      triedComponent(3), triedComponent(4), triedComponent(5),
      triedComponent(6), triedComponent(7), triedComponent(8))(List(_, _, _, _, _, _, _, _))

    triedValues.get should contain theSameElementsInOrderAs createComponents(8)
  }

  it should "create a representation from nine components" in {
    val triedValues = createRepresentation(triedComponent(1), triedComponent(2),
      triedComponent(3), triedComponent(4), triedComponent(5),
      triedComponent(6), triedComponent(7), triedComponent(8),
      triedComponent(9))(List(_, _, _, _, _, _, _, _, _))

    triedValues.get should contain theSameElementsInOrderAs createComponents(9)
  }

  it should "create a representation from ten components" in {
    val triedValues = createRepresentation(triedComponent(1), triedComponent(2),
      triedComponent(3), triedComponent(4), triedComponent(5),
      triedComponent(6), triedComponent(7), triedComponent(8),
      triedComponent(9), triedComponent(10))(List(_, _, _, _, _, _, _, _, _, _))

    triedValues.get should contain theSameElementsInOrderAs createComponents(10)
  }

  it should "create a representation from eleven components" in {
    val triedValues = createRepresentation(triedComponent(1), triedComponent(2),
      triedComponent(3), triedComponent(4), triedComponent(5),
      triedComponent(6), triedComponent(7), triedComponent(8),
      triedComponent(9), triedComponent(10),
      triedComponent(11))(List(_, _, _, _, _, _, _, _, _, _, _))

    triedValues.get should contain theSameElementsInOrderAs createComponents(11)
  }

  it should "create a representation from twelve components" in {
    val triedValues = createRepresentation(triedComponent(1), triedComponent(2),
      triedComponent(3), triedComponent(4), triedComponent(5),
      triedComponent(6), triedComponent(7), triedComponent(8),
      triedComponent(9), triedComponent(10),
      triedComponent(11), triedComponent(12))(List(_, _, _, _, _, _, _, _, _, _, _, _))

    triedValues.get should contain theSameElementsInOrderAs createComponents(12)
  }

  it should "create a representation from thirteen components" in {
    val triedValues = createRepresentation(triedComponent(1), triedComponent(2),
      triedComponent(3), triedComponent(4), triedComponent(5),
      triedComponent(6), triedComponent(7), triedComponent(8),
      triedComponent(9), triedComponent(10), triedComponent(11),
      triedComponent(12), triedComponent(13))(List(_, _, _, _, _, _, _, _, _, _, _, _, _))

    triedValues.get should contain theSameElementsInOrderAs createComponents(13)
  }

  it should "create a representation from fourteen components" in {
    val triedValues = createRepresentation(triedComponent(1), triedComponent(2),
      triedComponent(3), triedComponent(4), triedComponent(5),
      triedComponent(6), triedComponent(7), triedComponent(8),
      triedComponent(9), triedComponent(10), triedComponent(11),
      triedComponent(12), triedComponent(13),
      triedComponent(14))(List(_, _, _, _, _, _, _, _, _, _, _, _, _, _))

    triedValues.get should contain theSameElementsInOrderAs createComponents(14)
  }

  it should "create a representation from fifteen components" in {
    val triedValues = createRepresentation(triedComponent(1), triedComponent(2),
      triedComponent(3), triedComponent(4), triedComponent(5),
      triedComponent(6), triedComponent(7), triedComponent(8),
      triedComponent(9), triedComponent(10), triedComponent(11),
      triedComponent(12), triedComponent(13), triedComponent(14),
      triedComponent(15))(List(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _))

    triedValues.get should contain theSameElementsInOrderAs createComponents(15)
  }

  it should "support the access to input parameters" in {
    val parameters: Parameters = Map(ParameterParser.InputParameter -> List("1", "2", "3"))
    val extractor = inputValue(-2)
      .toInt
      .mandatory

    val result = runExtractor(extractor, parameters)
    result.get should be(2)
  }
}
