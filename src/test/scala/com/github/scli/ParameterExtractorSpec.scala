/*
 * Copyright 2020-2021 The Developers Team.
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

import com.github.scli.ParameterExtractor.{ExceptionGenerator, ExtractionContext, FailureCodes, OptionValue, Parameters}
import com.github.scli.ParameterModel.{ModelContext, ParameterKey}
import com.github.scli.ParameterParser.{OptionElement, ParametersMap}
import com.github.scli.ParametersTestHelper._
import org.mockito.Mockito._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.mockito.MockitoSugar

import scala.collection.SortedSet
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

object ParameterExtractorSpec {
  /** A test option key. */
  private val Key = "my-test-option"

  /** The ParameterKey corresponding to the test key. */
  private val TestParamKey = ParameterKey(Key, shortAlias = false)

  /** A result of a test CLI extractor. */
  private val ExtractorResult = 42

  /** The test value list assigned to the test key. */
  private val ResultValues = List(ExtractorResult.toString)

  /** Test data for input values passed on the command line. */
  private val InputValues = List("1", "2", "3")

  /** A test option value containing the test result. */
  private val ResultOptionValue: OptionValue[String] = Success(ResultValues)

  /** A test Parameters object for testing CLI extractors. */
  private val TestParameters: Parameters = Map(Key -> ResultValues)

  /** A test parameters object that contains input parameters. */
  private val TestParametersWithInputs: Parameters = TestParameters.parametersMap ++
    toParamValues(Map(ParameterParser.InputParameter -> InputValues))

  /** Another test Parameters object representing updated parameters. */
  private val NextParameters = generateParameters(1)

  /** A test exception generator function. */
  private val TestExceptionGenerator: ExceptionGenerator =
    (key, code, params) => new IllegalArgumentException(exceptionMessage(key, code, params))

  /** A test ExtractionContext object. */
  private val TestContext = ExtractionContext(TestParameters,
    new ModelContext(Map.empty, SortedSet.empty, ParameterModel.EmptyAliasMapping, None, Nil),
    DummyConsoleReader, TestExceptionGenerator, None)

  /**
   * Generates an exception message for the parameters specified. This function
   * is used by the test exception generator function.
   *
   * @param key    the parameter key
   * @param code   the failure code
   * @param params additional parameters
   * @return an exception message derived from these parameters
   */
  private def exceptionMessage(key: ParameterKey, code: FailureCodes.Value, params: Seq[String]): String =
    s"$key:$code:$params"

  /**
   * Returns a test ''Parameters'' instance based on the given index.
   *
   * @param index the index to generate dynamic parameters
   * @return the test ''Parameters'' with this index
   */
  private def generateParameters(index: Int): Parameters =
    Parameters(toParamValues(Map(pk(s"bar$index") -> List(s"v$index", s"v${index + 1}"))),
      Set(pk(s"x$index"), pk("y")))
}

/**
 * Test class for ''ParameterExtractor''.
 */
class ParameterExtractorSpec extends AnyFlatSpec with Matchers with MockitoSugar {

  import ParameterExtractor._
  import ParameterExtractorSpec._

  /**
   * Creates a default ''ExtractionContext'' that is used by tests to execute an
   * extractor.
   *
   * @param params the map with parameters
   * @param reader the console reader
   * @param mapper the optional exception mapper function
   * @return the ''ExtractionContext''
   */
  private def extractionContext(params: Parameters = TestParameters,
                                reader: ConsoleReader = mock[ConsoleReader],
                                mapper: Option[ExceptionMapper] = None): ExtractionContext =
    ExtractionContext(params, ParameterModel.EmptyModelContext, reader, TestExceptionGenerator, mapper)

  /**
   * Expects that the given ''Try'' is a failure wrapping a
   * ''ParameterExtractionException''. This exception is returned.
   *
   * @param res the tried result
   * @tparam T the result type of the ''Try''
   * @return the exception that was extracted
   */
  private def expectExtractionException[T](res: Try[T]): ParameterExtractionException =
    res match {
      case Failure(exception: ParameterExtractionException) => exception
      case r => fail("Unexpected result: " + r)
    }

  /**
   * Checks the properties of an ''ExtractionFailure''.
   *
   * @param failure     the failure object to be checked
   * @param expKey      the expected key
   * @param expParams   the expected parameters map
   * @param expMsgParts strings to be contained in the message
   * @param ct          the class tag for the exception type
   * @tparam E the expected type of the cause exception
   * @return the ''ExtractionFailure'' unchanged
   */
  private def checkExtractionFailure[E](failure: ExtractionFailure, expKey: ParameterKey = TestParamKey,
                                        expParams: ParametersMap = TestParameters.parametersMap)
                                       (expMsgParts: String*)
                                       (implicit ct: ClassTag[E]): ExtractionFailure = {

    failure.key should be(expKey)
    failure.context.parameters.parametersMap should be(expParams)
    failure.cause.getClass should be(ct.runtimeClass)
    expMsgParts foreach { part =>
      failure.cause.getMessage should include(part)
    }
    failure
  }

  /**
   * Checks the ''ExtractionFailure'' of a ''ParameterExtractionException''.
   * It is expected that the exception contains only a single failure. The
   * properties of this failure are checked.
   *
   * @param exception   the exception to be checked
   * @param expKey      the expected key
   * @param expParams   the expected parameters map
   * @param expMsgParts strings to be contained in the message
   * @param ct          the class tag for the exception type
   * @tparam E the expected type of the cause exception
   * @return the ''ExtractionFailure'' unchanged
   */
  private def checkExtractionException[E](exception: ParameterExtractionException,
                                          expKey: ParameterKey = TestParamKey,
                                          expParams: ParametersMap = NextParameters.parametersMap)
                                         (expMsgParts: String*)
                                         (implicit ct: ClassTag[E]): ExtractionFailure = {
    exception.failures should have size 1
    checkExtractionFailure[E](exception.failures.head, expKey, expParams)(expMsgParts: _*)
  }

  "Parameters" should "be creatable from a parameters map" in {
    val paramMap = toParamValues(Map(pk("foo") -> List("v1", "v2"), pk("bar") -> List("v3")))

    val params: Parameters = paramMap
    params.parametersMap should be(paramMap)
    params.accessedParameters should have size 0
    params.allKeysAccessed shouldBe false
  }

  it should "report an empty map as fully accessed" in {
    val params = Parameters(Map.empty, Set.empty)

    params.allKeysAccessed shouldBe true
  }

  it should "report parameters as fully consumed if the set contains more keys" in {
    val params = toParametersWithAccessed(Map("foo" -> List("v1")), Set("foo", "bar"))

    params.allKeysAccessed shouldBe true
  }

  it should "return the keys that have not been accessed" in {
    val params: Parameters = Map("foo" -> List("v1", "v2"), "bar" -> List("v3"), "baz" -> List("v4"))

    val params2 = params.keyAccessed(pk("baz"))
    params2.notAccessedKeys should contain only(pk("foo"), pk("bar"))
  }

  it should "support marking multiple keys as accessed" in {
    val params: Parameters = Map("foo" -> List("v1", "v2"), "bar" -> List("v3"), "baz" -> List("v4"),
      "blub" -> List("v5"))

    val params2 = params.keysAccessed(List(pk("baz"), pk("foo")))
    params2.notAccessedKeys should contain only(pk("bar"), pk("blub"))
  }

  it should "not create a new object if an already accessed key is marked as accessed" in {
    val params = toParametersWithAccessed(Map("foo" -> List("v")), Set("bar"))

    val params2 = params.keyAccessed(pk("bar"))
    params2 should be theSameInstanceAs params
  }

  "ParameterExtractionException" should "not allow creating an instance without failures" in {
    intercept[IllegalArgumentException] {
      ParameterExtractionException(Nil)
    }
  }

  it should "generate a message from the failures" in {
    val failure1 = ExtractionFailure(TestParamKey, new Exception("Message 1"), None, TestContext)
    val failure2 = ExtractionFailure(ParameterKey(Key + "_other", shortAlias = false),
      new IllegalStateException("Other message"), None, TestContext)
    val ExpMsg = failure1.key.key + ": " + failure1.cause.getMessage + ", " +
      failure2.key.key + ": " + failure2.cause.getMessage

    val exception = ParameterExtractionException(List(failure1, failure2))
    exception.getMessage should be(ExpMsg)
  }

  "ExtractionFailure" should "return the failure key if available" in {
    val FailureKey = ParameterKey("failureKey", shortAlias = false)
    val element = OptionElement(FailureKey, Some("a value"), 0)
    val failure = ExtractionFailure(TestParamKey, new Exception, Some(element), TestContext)

    failure.failureKey should be(FailureKey)
  }

  it should "return the main key as failure key if no element is available" in {
    val failure = ExtractionFailure(TestParamKey, new Exception, None, TestContext)

    failure.failureKey should be(TestParamKey)
  }

  it should "return the original value if available" in {
    val Value = "originalValue"
    val element = OptionElement(TestParamKey, Some(Value), 0)
    val failure = ExtractionFailure(TestParamKey, new Exception, Some(element), TestContext)

    failure.optOriginalValue should be(Some(Value))
  }

  it should "return no original value if no element is available" in {
    val failure = ExtractionFailure(TestParamKey, new Exception, None, TestContext)

    failure.optOriginalValue should be(None)
  }

  /**
   * Creates a generic test Cli extractor that checks the context passed to it
   * and returns a defined result.
   *
   * @param value          the value to be returned by the extractor
   * @param expExtrCtx     the expected extraction context
   * @param nextParameters the updated parameters
   * @param key            the parameter key of the test extractor
   * @tparam A the type of the value
   * @return the test extractor
   */
  private def testExtractor[A](value: A, expExtrCtx: ExtractionContext, nextParameters: Parameters = NextParameters,
                               key: ParameterKey = TestParamKey): CliExtractor[A] = CliExtractor(context => {
    context.parameters should be(expExtrCtx.parameters)
    context.reader should be(expExtrCtx.reader)
    (value, context.update(nextParameters, context.modelContext))
  }, Some(key))

  "ParameterExtractor" should "support running a CliExtractor" in {
    val context = extractionContext()
    val ext = testExtractor(ExtractorResult, context)

    val (res, next) = ParameterExtractor.runExtractor(ext, context)
    res should be(ExtractorResult)
    next.parameters should be(NextParameters)
  }

  it should "run an extractor yielding a Try if execution is successful" in {
    val context = extractionContext()
    val ext = testExtractor[Try[Int]](Success(ExtractorResult), context)

    ParameterExtractor.tryExtractor(ext, context) match {
      case Success((res, next)) =>
        res should be(ExtractorResult)
        next.parameters should be(NextParameters)
      case f => fail("Unexpected result: " + f)
    }
  }

  it should "run an extractor yielding a Try if execution fails" in {
    val exception = new IllegalArgumentException("Wrong parameters")
    val context = extractionContext()
    val ext = testExtractor[Try[Int]](Failure(exception), context)

    ParameterExtractor.tryExtractor(ext, context) match {
      case Failure(ex) =>
        ex should be(exception)
      case s => fail("Unexpected result: " + s)
    }
  }

  it should "wrap a function in a Try" in {
    val triedResult = ParameterExtractor.paramTry(TestContext, TestParamKey)(ExtractorResult)

    triedResult should be(Success(ExtractorResult))
  }

  it should "catch the exception thrown by a function and wrap it" in {
    val exception = new IOException("Fatal error")

    val triedResult = ParameterExtractor.paramTry[String](TestContext, TestParamKey)(throw exception)
    checkExtractionException[IOException](expectExtractionException(triedResult),
      expParams = TestParameters.parametersMap)(exception.getMessage)
  }

  it should "handle a ParameterExtractionException thrown within a Try in a special way" in {
    val exception = ParameterExtractionException(ExtractionFailure(TestParamKey, new Exception("Some error"),
      None, TestContext))

    val triedResult = ParameterExtractor.paramTry[String](TestContext, TestParamKey)(throw exception)
    expectExtractionException(triedResult) should be theSameInstanceAs exception
  }

  it should "provide a constant extractor" in {
    val extractor = ParameterExtractor.constantExtractor(ExtractorResult)

    val (res, next) = ParameterExtractor.runExtractor(extractor, extractionContext())
    res should be(ExtractorResult)
    next.parameters should be(TestParameters)
  }

  it should "provide an extractor returning a constant option value with only a single value" in {
    val extractor = ParameterExtractor.constantOptionValue(ExtractorResult.toString)

    val (res, next) = ParameterExtractor.runExtractor(extractor, extractionContext())
    next.parameters should be(TestParameters)
    res.get should contain only ExtractorResult.toString
  }

  it should "provide an extractor returning a constant option value with multiple values" in {
    val items = List("foo", "bar", "baz", "more")
    val extractor = ParameterExtractor.constantOptionValue(items.head, items.tail: _*)

    val (res, next) = ParameterExtractor.runExtractor(extractor, extractionContext())
    next.parameters should be(TestParameters)
    res.get should be(items)
  }

  it should "provide an extractor to extract a single option value if there is exactly one value" in {
    val context = extractionContext()
    val ext = testExtractor(ResultOptionValue, context)
    val extractor = ParameterExtractor.asSingleOptionValue(ext)

    val (res, next) = ParameterExtractor.runExtractor(extractor, context)
    next.parameters should be(NextParameters)
    res should be(Success(Some(ExtractorResult.toString)))
  }

  it should "provide an extractor to extract a single option value if the value is undefined" in {
    val context = extractionContext()
    val EmptyValue: OptionValue[String] = Success(Nil)
    val ext = testExtractor(EmptyValue, context)
    val extractor = ParameterExtractor.asSingleOptionValue(ext)

    val (res, next) = ParameterExtractor.runExtractor(extractor, context)
    next.parameters should be(NextParameters)
    res should be(Success(None))
  }

  it should "provide an extractor to extract a single option value if there are multiple values" in {
    val context = extractionContext()
    val MultiValue: OptionValue[String] = Success(List("v1", "v2"))
    val ext = testExtractor(MultiValue, context)
    val extractor = ParameterExtractor.asSingleOptionValue(ext)

    val (res, next) = ParameterExtractor.runExtractor(extractor, context)
    next.parameters should be(NextParameters)
    val paramEx = expectExtractionException(res)
    checkExtractionException[IllegalArgumentException](paramEx)(exceptionMessage(TestParamKey,
      FailureCodes.MultipleValues, Seq("v1, v2")))
  }

  it should "provide an extractor to extract a single option value if there are multiple values with override" in {
    val context = extractionContext()
    val MultiValue: OptionValue[String] = Success(List("v1", "v2"))
    val ext = testExtractor(MultiValue, context)
    val extractor = ParameterExtractor.asSingleOptionValue(ext, allowOverride = true)

    val (res, next) = ParameterExtractor.runExtractor(extractor, context)
    next.parameters should be(NextParameters)
    res should be(Success(Some("v2")))
  }

  it should "provide a mapping extractor that handles a failed result" in {
    val context = extractionContext()
    val FailedValue: OptionValue[String] = Failure(new Exception("Failed"))
    val ext = testExtractor(FailedValue, context)
    val extractor = ParameterExtractor.mapped(ext)(_.toInt)

    val (res, next) = ParameterExtractor.runExtractor(extractor, context)
    next.parameters should be(NextParameters)
    res should be(FailedValue)
  }

  it should "provide a mapping extractor that handles an empty result" in {
    val context = extractionContext()
    val EmptyResult: OptionValue[String] = Success(None)
    val ext = testExtractor(EmptyResult, context)
    val extractor = ParameterExtractor.mapped(ext)(_ => throw new IllegalArgumentException("Nope"))

    val (res, next) = ParameterExtractor.runExtractor(extractor, context)
    next.parameters should be(NextParameters)
    res should be(EmptyResult)
  }

  it should "provide a mapping extractor that handles a defined result" in {
    val context = extractionContext()
    val Result: OptionValue[String] = Success(Some(ExtractorResult.toString))
    val ext = testExtractor(Result, context)
    val extractor = ParameterExtractor.mapped(ext)(_.toInt)

    val (res, next) = ParameterExtractor.runExtractor(extractor, context)
    next.parameters should be(NextParameters)
    res should be(Success(List(ExtractorResult)))
  }

  it should "provide a mapping extractor that handles an exception thrown by the mapping function" in {
    val context = extractionContext()
    val elem = OptionElement(ParameterKey("err", shortAlias = true), Some("wrong value"), 0)
    val params: Parameters = Map(TestParamKey -> List(elem))
    val InvalidNumber = "Not a number!"
    val Result: OptionValue[String] = Success(Some(InvalidNumber))
    val ext = testExtractor(Result, context, nextParameters = params)
    val extractor = ParameterExtractor.mapped(ext)(_.toInt)

    val (res, next) = ParameterExtractor.runExtractor(extractor, context)
    next.parameters should be(params)
    val failure = checkExtractionException[NumberFormatException](expectExtractionException(res),
      expParams = params.parametersMap)(InvalidNumber)
    failure.optElement should be(Some(elem))
  }

  it should "provide a mapping extractor that passes the ExtractionContext to the mapping function" in {
    val context = extractionContext()
    val IntValues = List(17, 21, 44, 127)
    val StrValues = IntValues map (_.toString)
    val Result: OptionValue[Int] = Success(IntValues)
    val ext = testExtractor(Result, context)
    val extractor = ParameterExtractor.mappedWithContext(ext) { (i, ctx) =>
      val res = i.toString
      val nextHelpCtx = ctx.modelContext.addOption(ParameterKey(res, shortAlias = false), None)
      (res, ctx.copy(modelContext = nextHelpCtx))
    }

    val (res, next) = ParameterExtractor.runExtractor(extractor, context)
    next.parameters should be(NextParameters)
    res should be(Success(StrValues))
    val keys = next.modelContext.options.keys map (_.key)
    keys should contain allOf(StrValues.head, StrValues.tail.head, StrValues.drop(2): _*)
  }

  it should "provide a mapping extractor that collects multiple mapping errors" in {
    val context = extractionContext()
    val InvalidValues = List("xy", "noNumber", "1234abc")
    val Values = List("17", InvalidValues.head, "21", "44", InvalidValues(1), InvalidValues(2), "127")
    val Alias = ParameterKey("e", shortAlias = true)
    val elements = Values.zipWithIndex map { t =>
      val key = if (t._2 % 2 == 0) TestParamKey else Alias
      OptionElement(key, Some(t._1), t._2)
    }
    val failureElements = elements filter (e => InvalidValues contains e.value)
    val params: Parameters = Map(TestParamKey -> elements)
    val Result: OptionValue[String] = Success(Values)
    val ext = testExtractor(Result, context, nextParameters = params)
    val extractor = ParameterExtractor.mapped(ext)(_.toInt)

    val (res, next) = ParameterExtractor.runExtractor(extractor, context)
    next.parameters should be(params)
    val exception = expectExtractionException(res)
    exception.failures should have size InvalidValues.size
    exception.failures.zip(InvalidValues).zip(failureElements) foreach { t =>
      val failure = checkExtractionFailure[NumberFormatException](t._1._1, expParams = params.parametersMap)(t._1._2)
      failure.optElement should be(Some(t._2))
    }
  }

  it should "provide a mapping extractor that deals with insufficient error information" in {
    val context = extractionContext()
    val params: Parameters = Map(TestParamKey -> List(OptionElement(TestParamKey, None, 0)))
    val Result: OptionValue[String] = Success(List("42", "foo"))
    val ext = testExtractor(Result, context, nextParameters = params)
    val extractor = ParameterExtractor.mapped(ext)(_.toInt)

    val (res, next) = ParameterExtractor.runExtractor(extractor, context)
    next.parameters should be(params)
    val failure = checkExtractionException[NumberFormatException](expectExtractionException(res),
      expParams = params.parametersMap)()
    failure.optElement should be(None)
  }

  it should "provide a mapping extractor that invokes the exception mapper function" in {
    val mapper: ExceptionMapper = (key, optElem) => {
      case _: NumberFormatException =>
        new IllegalArgumentException(exceptionMessage(key, FailureCodes.UnsupportedParameter, Seq(optElem.get.value)))
    }
    val Value = "notANumber"
    val context = extractionContext(mapper = Some(mapper))
    val params: Parameters = Map(TestParamKey -> List(OptionElement(TestParamKey, Some("1"), 0),
      OptionElement(TestParamKey, Some(Value), 1), OptionElement(TestParamKey, Some("2"), 2)))
    val Result: OptionValue[String] = Success(List("1", Value, "2"))
    val ext = testExtractor(Result, context, nextParameters = params)
    val extractor = ParameterExtractor.mapped(ext)(_.toInt)

    val (res, _) = ParameterExtractor.runExtractor(extractor, context)
    checkExtractionException[IllegalArgumentException](expectExtractionException(res),
      expParams = params.parametersMap)(exceptionMessage(TestParamKey, FailureCodes.UnsupportedParameter, Seq(Value)))
  }

  it should "provide a mapping extractor that correctly invokes the partial exception mapper function" in {
    val mapper: ExceptionMapper = (_, _) => {
      case _: IllegalStateException => new UnsupportedOperationException
    }
    val Value = "strangeNumber"
    val context = extractionContext(mapper = Some(mapper))
    val Result: OptionValue[String] = Success(List(Value))
    val ext = testExtractor(Result, context)
    val extractor = ParameterExtractor.mapped(ext)(_.toInt)

    val (res, _) = ParameterExtractor.runExtractor(extractor, context)
    checkExtractionException[NumberFormatException](expectExtractionException(res))(Value)
  }

  it should "provide a single value mapping extractor that handles a failed result" in {
    val context = extractionContext()
    val FailedValue: SingleOptionValue[String] = Failure(new Exception("Failed"))
    val ext = testExtractor(FailedValue, context)
    val extractor = ParameterExtractor.mappedSingle(ext)(_.toInt)

    val (res, next) = ParameterExtractor.runExtractor(extractor, context)
    next.parameters should be(NextParameters)
    res should be(FailedValue)
  }

  it should "provide a single mapping extractor that handles an empty result" in {
    val context = extractionContext()
    val EmptyResult: SingleOptionValue[String] = Success(None)
    val ext = testExtractor(EmptyResult, context)
    val extractor = ParameterExtractor.mappedSingle(ext)(_ => throw new IllegalArgumentException("Nope"))

    val (res, next) = ParameterExtractor.runExtractor(extractor, context)
    next.parameters should be(NextParameters)
    res should be(EmptyResult)
  }

  it should "provide a single mapping extractor that handles a defined result" in {
    val context = extractionContext()
    val Result: SingleOptionValue[String] = Success(Some(ExtractorResult.toString))
    val ext = testExtractor(Result, context)
    val extractor = ParameterExtractor.mappedSingle(ext)(_.toInt)

    val (res, next) = ParameterExtractor.runExtractor(extractor, context)
    next.parameters should be(NextParameters)
    res should be(Success(Some(ExtractorResult)))
  }

  it should "provide a single mapping extractor that handles an exception thrown by the mapping function" in {
    val context = extractionContext()
    val InvalidNumber = "Not a number!"
    val Result: SingleOptionValue[String] = Success(Some(InvalidNumber))
    val elem = OptionElement(pk("someAlternativeKey"), Some("foo"), 0)
    val params: Parameters = Map(TestParamKey -> List(elem))
    val ext = testExtractor(Result, context, nextParameters = params)
    val extractor = ParameterExtractor.mappedSingle(ext)(_.toInt)

    val (res, next) = ParameterExtractor.runExtractor(extractor, context)
    next.parameters should be(params)
    val failure = checkExtractionException[NumberFormatException](expectExtractionException(res),
      expParams = params.parametersMap)(InvalidNumber)
    failure.optElement should be(Some(elem))
  }

  it should "provide a single mapping extractor that handles exceptions and too much error information" in {
    val context = extractionContext()
    val Result: SingleOptionValue[String] = Success(Some("Not a number!"))
    val params: Parameters = Map(TestParamKey -> List(OptionElement(TestParamKey, Some("v1"), 0),
      OptionElement(pk("other"), Some("v2"), 0)))
    val ext = testExtractor(Result, context, nextParameters = params)
    val extractor = ParameterExtractor.mappedSingle(ext)(_.toInt)

    val (res, _) = ParameterExtractor.runExtractor(extractor, context)
    val failure = checkExtractionException[NumberFormatException](expectExtractionException(res),
      expParams = params.parametersMap)()
    failure.optElement should be(None)
  }

  it should "provide a single mapping extractor that invokes the exception mapper function" in {
    val mapper: ExceptionMapper = (key, optElem) => {
      case e: NumberFormatException =>
        new IllegalArgumentException(exceptionMessage(key, FailureCodes.UnsupportedParameter,
          Seq(optElem.get.value)), e)
    }
    val context = extractionContext(mapper = Some(mapper))
    val InvalidNumber = "Not a number!"
    val Result: SingleOptionValue[String] = Success(Some(InvalidNumber))
    val elem = OptionElement(pk("someAlternativeKey"), Some(InvalidNumber), 0)
    val params: Parameters = Map(TestParamKey -> List(elem))
    val ext = testExtractor(Result, context, nextParameters = params)
    val extractor = ParameterExtractor.mappedSingle(ext)(_.toInt)

    val (res, next) = ParameterExtractor.runExtractor(extractor, context)
    next.parameters should be(params)
    val failure = checkExtractionException[IllegalArgumentException](expectExtractionException(res),
      expParams = params.parametersMap)(exceptionMessage(TestParamKey, FailureCodes.UnsupportedParameter,
      Seq(InvalidNumber)))
    failure.cause.getCause shouldBe a[NumberFormatException]
  }

  it should "provide an extractor that converts an option value to int" in {
    val context = extractionContext()
    val StrValue: OptionValue[String] = Try(Some(ExtractorResult.toString))
    val ext = testExtractor(StrValue, context)
    val extractor = ext.toInt

    val (res, next) = ParameterExtractor.runExtractor(extractor, context)
    next.parameters should be(NextParameters)
    res should be(Success(List(ExtractorResult)))
  }

  it should "provide an extractor that converts an option value to int and handles errors" in {
    val context = extractionContext()
    val NoIntValue = "not a valid number"
    val StrValue: OptionValue[String] = Try(Some(NoIntValue))
    val ext = testExtractor(StrValue, context)
    val extractor = ext.toInt

    val (res, next) = ParameterExtractor.runExtractor(extractor, context)
    next.parameters should be(NextParameters)
    checkExtractionException[NumberFormatException](expectExtractionException(res))(NoIntValue)
  }

  /**
   * Helper method for testing a boolean conversion.
   *
   * @param value     the original string option value
   * @param expResult the expected result
   */
  private def checkBooleanConversion(value: String, expResult: Boolean): Unit = {
    val context = extractionContext()
    val StrValue: OptionValue[String] = Try(Some(value))
    val ext = testExtractor(StrValue, context)
    val extractor = ext.toBoolean

    val (res, next) = ParameterExtractor.runExtractor(extractor, context)
    next.parameters should be(NextParameters)
    res should be(Success(List(expResult)))
  }

  it should "provide an extractor that converts an option to boolean if the result is true" in {
    checkBooleanConversion("true", expResult = true)
  }

  it should "provide an extractor that converts an option to boolean if the result is false" in {
    checkBooleanConversion("false", expResult = false)
  }

  it should "provide an extractor that converts an option to boolean and handles errors" in {
    val context = extractionContext()
    val StrValue = "not a valid boolean"
    val ValueOption: OptionValue[String] = Try(Some(StrValue))
    val ext = testExtractor(ValueOption, context)
    val extractor = ext.toBoolean

    val (res, next) = ParameterExtractor.runExtractor(extractor, context)
    next.parameters should be(NextParameters)
    checkExtractionException[IllegalArgumentException](expectExtractionException(res))(StrValue)
  }

  it should "provide an extractor that converts string values to lower case" in {
    val context = extractionContext()
    val ValueOption: OptionValue[String] = Try(Some("This Is a TEST String"))
    val ext = testExtractor(ValueOption, context)
    val extractor = ext.toLower

    val (res, next) = ParameterExtractor.runExtractor(extractor, context)
    next.parameters should be(NextParameters)
    res should be(Success(List("this is a test string")))
  }

  it should "provide an extractor that converts string values to upper case" in {
    val context = extractionContext()
    val ValueOption: OptionValue[String] = Try(Some("This Is a TEST String"))
    val ext = testExtractor(ValueOption, context)
    val extractor = ext.toUpper

    val (res, next) = ParameterExtractor.runExtractor(extractor, context)
    next.parameters should be(NextParameters)
    res should be(Success(List("THIS IS A TEST STRING")))
  }

  it should "provide an extractor that does a mapping of enum values" in {
    val context = extractionContext()
    val mapping = Map("foo" -> 1, "bar" -> 2, "baz" -> 3)
    val values = mapping.keys.toList
    val results = values map (mapping(_))
    val ValueOption: OptionValue[String] = Success(values)
    val ext = testExtractor(ValueOption, context)
    val extractor = ext.toEnum(mapping.get)

    val (res, next) = ParameterExtractor.runExtractor(extractor, context)
    next.parameters should be(NextParameters)
    res.get.toList should contain theSameElementsInOrderAs results
  }

  it should "provide an enum extractor that handles invalid literals" in {
    val context = extractionContext()
    val mappingFunc: String => Option[Int] = _ => None
    val Value = "foo"
    val ValueOption: OptionValue[String] = Try(Some(Value))
    val ext = testExtractor(ValueOption, context)
    val extractor = ext.toEnum(mappingFunc)

    val (res, next) = ParameterExtractor.runExtractor(extractor, context)
    next.parameters should be(NextParameters)
    checkExtractionException[IllegalArgumentException](expectExtractionException(res))("enum", Value)
  }

  it should "provide an extractor that returns a mandatory value" in {
    val context = extractionContext()
    val ValueOption: SingleOptionValue[Int] = Success(Some(ExtractorResult))
    val ext = testExtractor(ValueOption, context)
    val extractor = ParameterExtractor.asMandatory(ext)

    val (res, next) = ParameterExtractor.runExtractor(extractor, context)
    next.parameters should be(NextParameters)
    res should be(Success(ExtractorResult))
  }

  it should "provide an extractor that fails if an option does not have a value" in {
    val context = extractionContext()
    val ValueOption: SingleOptionValue[Int] = Success(None)
    val ext = testExtractor(ValueOption, context)
    val extractor = ParameterExtractor.asMandatory(ext)

    val (res, next) = ParameterExtractor.runExtractor(extractor, context)
    next.parameters should be(NextParameters)
    checkExtractionException[IllegalArgumentException](expectExtractionException(res))(exceptionMessage(TestParamKey,
      FailureCodes.MandatoryParameterMissing, Seq.empty))
  }

  it should "provide an extractor that fails if the multiplicity of a parameter is too low" in {
    val context = extractionContext()
    val Values: OptionValue[Int] = Success(List(1))
    val ext = testExtractor(Values, context)
    val extractor = ParameterExtractor.withMultiplicity(ext, 2, -1)

    val (res, next) = ParameterExtractor.runExtractor(extractor, context)
    next.parameters should be(NextParameters)
    checkExtractionException[IllegalArgumentException](expectExtractionException(res))(exceptionMessage(TestParamKey,
      FailureCodes.MultiplicityTooLow, Seq("2")))
  }

  it should "provide an extractor that fails if the multiplicity of a parameter is too high" in {
    val context = extractionContext()
    val Values: OptionValue[Int] = Success(List(1, 2, 3))
    val ext = testExtractor(Values, context)
    val extractor = ParameterExtractor.withMultiplicity(ext, 1, 2)

    val (res, next) = ParameterExtractor.runExtractor(extractor, context)
    next.parameters should be(NextParameters)
    checkExtractionException[IllegalArgumentException](expectExtractionException(res))(exceptionMessage(TestParamKey,
      FailureCodes.MultiplicityTooHigh, Seq("2")))
  }

  it should "provide an extractor that reads from the console" in {
    val consoleReader: ConsoleReader = mock[ConsoleReader]
    val context = extractionContext(reader = consoleReader)
    val Result = "enteredFromUser"
    when(consoleReader.readOption(Key, password = true)).thenReturn(Result)
    val extractor = ParameterExtractor.consoleReaderValue(Key)

    val (res, next) = ParameterExtractor.runExtractor(extractor, context)
    next.parameters should be(TestParameters)
    res.get should be(Some(Result))
  }

  it should "evaluate the password flag of the console reader extractor" in {
    val consoleReader: ConsoleReader = mock[ConsoleReader]
    val context = extractionContext(reader = consoleReader)
    val Result = "enteredFromUser"
    when(consoleReader.readOption(Key, password = false)).thenReturn(Result)
    val extractor = ParameterExtractor.consoleReaderValue(Key, password = false)

    val (res, next) = ParameterExtractor.runExtractor(extractor, context)
    next.parameters should be(TestParameters)
    res.get should be(Some(Result))
  }

  it should "evaluate the prompt when reading an option from the command line" in {
    val consoleReader: ConsoleReader = mock[ConsoleReader]
    val context = extractionContext(reader = consoleReader)
    val Prompt = "Dear user, please be so kind to enter the option value"
    val Result = "enteredFromUserAfterNicePrompt"
    when(consoleReader.readOption(Prompt, password = false)).thenReturn(Result)
    val extractor = ParameterExtractor.consoleReaderValue(Key, password = false, optPrompt = Some(Prompt))

    val (res, next) = ParameterExtractor.runExtractor(extractor, context)
    next.parameters should be(TestParameters)
    res.get should be(Some(Result))
  }

  it should "provide an extractor that yields an empty option value" in {
    val (res, next) = ParameterExtractor.runExtractor(ParameterExtractor.emptyExtractor[Int], extractionContext())

    next.parameters should be(TestParameters)
    res should be(ParameterExtractor.emptyOptionValue)
    res.get should have size 0
  }

  it should "provide a conditional extractor that executes the if case" in {
    val context = extractionContext()
    val context2 = context.copy(parameters = NextParameters)
    val nextNextParameters = toParametersWithAccessed(Map("next" -> List("v4", "v5")), Set("x", "y", "z"))
    val condExt: CliExtractor[Try[Boolean]] = testExtractor(Success(true), context)
    val ifExt = testExtractor(ResultOptionValue, context2, nextParameters = nextNextParameters)
    val extractor = ParameterExtractor.conditionalOptionValue(condExt, ifExt)

    val (res, next) = ParameterExtractor.runExtractor(extractor, context)
    next.parameters should be(nextNextParameters)
    res should be(ResultOptionValue)
  }

  it should "provide a condition extractor that executes the else case" in {
    val context = extractionContext()
    val context2 = context.copy(parameters = NextParameters)
    val nextNextParameters = toParametersWithAccessed(Map("next" -> List("v4", "v5")), Set("x", "y", "z"))
    val condExt: CliExtractor[Try[Boolean]] = testExtractor(Success(false), context)
    val elseExt = testExtractor(ResultOptionValue, context2, nextParameters = nextNextParameters)
    val extractor = ParameterExtractor.conditionalOptionValue(condExt, ParameterExtractor.emptyExtractor, elseExt)

    val (res, next) = ParameterExtractor.runExtractor(extractor, context)
    next.parameters should be(nextNextParameters)
    res should be(ResultOptionValue)
  }

  it should "provide a condition extractor that handles failures" in {
    val context = extractionContext()
    val Key2 = pk("keyElse")
    val exception = new Exception("failed")
    val condExt: CliExtractor[Try[Boolean]] = testExtractor(Failure(exception), context)
    val ifExt = optionValues(Key)
    val elseExt = optionValues(Key2.key)
    val extractor = ParameterExtractor.conditionalOptionValue(condExt, ifExt = ifExt, elseExt = elseExt)

    val (res, next) = ParameterExtractor.runExtractor(extractor, context)
    next.parameters should be(NextParameters)
    next.modelContext.options.keys should contain allElementsOf List(Key2, TestParamKey)
    res should be(Failure(exception))
  }

  it should "provide a conditional group extractor that executes the correct group" in {
    val context = extractionContext()
    val context2 = context.copy(parameters = NextParameters)
    val nextNextParameters = toParametersWithAccessed(Map("next" -> List("v4", "v5")), Set("x", "y", "z"))
    val groupExt: CliExtractor[Try[String]] = testExtractor(Success("foo"), context)
    val activeExt = testExtractor(ResultOptionValue, context2, nextParameters = nextNextParameters)
    val otherExt = constantOptionValue("anotherResult")
    val groupMap = Map("foo" -> activeExt, "bar" -> otherExt)
    val extractor = conditionalGroupValue(groupExt, groupMap)

    val (res, next) = ParameterExtractor.runExtractor(extractor, context)
    next.parameters should be(nextNextParameters)
    res should be(ResultOptionValue)
  }

  it should "provide a conditional group extractor that handles a failure of the group selector" in {
    val context = extractionContext()
    val failedResult = Failure(new Exception("failure group"))
    val groupExt: CliExtractor[Try[String]] = testExtractor(failedResult, context)
    val groupMap = Map("foo" -> optionValue(Key))
    val extractor = conditionalGroupValue(groupExt, groupMap)

    val (res, next) = ParameterExtractor.runExtractor(extractor, context)
    next.parameters should be(NextParameters)
    res should be(failedResult)
    next.modelContext.options.keys should contain(TestParamKey)
  }

  it should "provide a conditional group extractor that fails if the group cannot be resolved" in {
    val context = extractionContext()
    val GroupName = "foo"
    val groupExt: CliExtractor[Try[String]] = testExtractor(Success(GroupName), context)
    val groupMap = Map("bar" -> optionValue(Key), "baz" -> optionValue("other"))
    val extractor = conditionalGroupValue(groupExt, groupMap)

    val (res, next) = ParameterExtractor.runExtractor(extractor, context)
    next.parameters should be(NextParameters)
    next.modelContext.options.keys should contain(TestParamKey)
    checkExtractionException[IllegalArgumentException](expectExtractionException(res),
      expParams = TestParameters.parametersMap)(exceptionMessage(TestParamKey, FailureCodes.UnknownGroup,
      Seq(GroupName, "bar, baz")))
  }

  it should "provide an extractor that checks whether an option is defined if the option has a value" in {
    val extractor = ParameterExtractor.isDefinedExtractor(Key)

    val (res, next) = ParameterExtractor.runExtractor(extractor, extractionContext())
    next.parameters.accessedParameters should contain only TestParamKey
    res should be(Success(true))
  }

  it should "provide an extractor that checks whether an option is defined if the option has no value" in {
    val OtherKey = pk("undefinedOption")
    val extractor = ParameterExtractor.isDefinedExtractor(OtherKey.key)

    val (res, next) = ParameterExtractor.runExtractor(extractor, extractionContext())
    next.parameters.accessedParameters should contain only OtherKey
    res should be(Success(false))
  }

  it should "provide an extractor that extracts a single input value" in {
    val context = extractionContext(params = TestParametersWithInputs)
    val extractor = ParameterExtractor.inputValue(0)

    val (res, next) = ParameterExtractor.runExtractor(extractor, context)
    next.parameters.accessedParameters should contain only ParameterParser.InputParameter
    res.get should be(Some(InputValues.head))
  }

  it should "provide an extractor that extracts multiple input values" in {
    val context = extractionContext(params = TestParametersWithInputs)
    val extractor = ParameterExtractor.inputValues(0, 1)

    val (res, next) = ParameterExtractor.runExtractor(extractor, context)
    next.parameters.accessedParameters should contain only ParameterParser.InputParameter
    res.get.toList should contain theSameElementsInOrderAs InputValues.take(2)
  }

  it should "provide an extractor that extracts multiple input values and handles the last check" in {
    val context = extractionContext(params = TestParametersWithInputs)
    val extractor = ParameterExtractor.inputValues(0, InputValues.size - 1, last = true)

    val (res, next) = ParameterExtractor.runExtractor(extractor, context)
    next.parameters.accessedParameters should contain only ParameterParser.InputParameter
    res.get.toList should contain theSameElementsInOrderAs InputValues
  }

  it should "interpret a negative value for the to index of an input value" in {
    val context = extractionContext(params = TestParametersWithInputs)
    val extractor = ParameterExtractor.inputValues(1, -1)

    val (res, _) = ParameterExtractor.runExtractor(extractor, context)
    res.get.toList should contain theSameElementsInOrderAs InputValues.drop(1)
  }

  it should "interpret a negative value for the from index of an input value" in {
    val context = extractionContext(params = TestParametersWithInputs)
    val extractor = ParameterExtractor.inputValue(-2)

    val (res, _) = ParameterExtractor.runExtractor(extractor, context)
    res.get should be(Some(InputValues(1)))
  }

  it should "yield a failure if the index of an input value is too small" in {
    val context = extractionContext(params = TestParametersWithInputs)
    val extractor = ParameterExtractor.inputValue(-InputValues.size - 1)

    val (res, _) = ParameterExtractor.runExtractor(extractor, context)
    checkExtractionException[IllegalArgumentException](expectExtractionException(res),
      expKey = ParameterParser.InputParameter,
      expParams = TestParametersWithInputs.parametersMap)(exceptionMessage(ParameterParser.InputParameter,
      FailureCodes.MandatoryParameterMissing, Seq.empty))
  }

  it should "yield a failure if the index of an input value is too big" in {
    val context = extractionContext(params = TestParametersWithInputs)
    val paramKey = ParameterKey("my-input", shortAlias = false, hasPrefix = false)
    val extractor = ParameterExtractor.inputValues(1, InputValues.size, optKey = Some(paramKey.key))

    val (res, _) = ParameterExtractor.runExtractor(extractor, context)
    checkExtractionException[IllegalArgumentException](expectExtractionException(res), expKey = paramKey,
      expParams = TestParametersWithInputs.parametersMap)(exceptionMessage(paramKey,
      FailureCodes.MandatoryParameterMissing, Seq.empty))
  }

  it should "yield a failure if too many input parameters have been specified" in {
    val context = extractionContext(params = TestParametersWithInputs)
    val extractor = ParameterExtractor.inputValue(1, last = true)

    val res = ParameterExtractor.tryExtractor(extractor, context)
    checkExtractionException[IllegalArgumentException](expectExtractionException(res),
      expKey = ParameterParser.InputParameter,
      expParams = TestParametersWithInputs.parametersMap)(exceptionMessage(ParameterParser.InputParameter,
      FailureCodes.TooManyInputParameters, Seq("2")))
    ParameterParser.InputParameter.hasPrefix shouldBe false
  }

  it should "use the correct key in a failure that too many input parameters have been specified" in {
    val context = extractionContext(params = TestParametersWithInputs)
    val paramKey = ParameterKey("someInput", hasPrefix = false, shortAlias = false)
    val extractor = ParameterExtractor.inputValue(1, optKey = Some(paramKey.key), last = true)

    val res = ParameterExtractor.tryExtractor(extractor, context)
    checkExtractionException[IllegalArgumentException](expectExtractionException(res), expKey = paramKey,
      expParams = TestParametersWithInputs.parametersMap)(exceptionMessage(paramKey,
      FailureCodes.TooManyInputParameters, Seq("2")))
  }

  it should "store the key for input parameters" in {
    val paramKey = Some("myInput")
    val extractor = ParameterExtractor.inputValue(1, optKey = paramKey)

    extractor.key should be(ParameterKey(paramKey.get, shortAlias = false, hasPrefix = false))
  }

  it should "provide an extractor for switches" in {
    val args: Parameters = Map(Key -> List("true"))
    val context = extractionContext(params = args)
    val extractor = ParameterExtractor.switchValue(Key)

    val (res, next) = ParameterExtractor.runExtractor(extractor, context)
    next.parameters.accessedParameters should contain only TestParamKey
    res should be(Success(true))
  }

  it should "record the key in the extractor for switches" in {
    val extractor = switchValue(Key)

    extractor.key.key should be(Key)
  }

  it should "provide an extractor for switches that defines a fallback value" in {
    val extractor = ParameterExtractor.switchValue("flag")

    val (res, _) = ParameterExtractor.runExtractor(extractor, extractionContext())
    res should be(Success(false))
  }

  it should "provide an extractor for switches that allows overriding the fallback value" in {
    val extractor = ParameterExtractor.switchValue("flag", presentValue = false)

    val (res, _) = ParameterExtractor.runExtractor(extractor, extractionContext())
    res should be(Success(true))
  }

  it should "support adding aliases to an extractor" in {
    val AliasLong = "otherNameFor" + Key
    val AliasShort = "f"
    val ext = ParameterExtractor.switchValue(Key, presentValue = false)
    val extractor = ParameterExtractor.withAlias(ParameterExtractor.withAlias(ext, AliasShort),
      AliasLong, shortAlias = false)

    val (_, ctx) = ParameterExtractor.runExtractor(extractor, extractionContext())
    ctx.modelContext.aliasMapping.aliasesForKey(TestParamKey) should contain only(pk(AliasLong),
      ParameterKey(AliasShort, shortAlias = true))
  }

  it should "provide an extractor defining excluding options that handles the case that no option is defined" in {
    val context1 = extractionContext()
    val context2 = extractionContext(NextParameters, reader = context1.reader)
    val finalParameters = generateParameters(2)
    val value: SingleOptionValue[String] = Success(None)
    val ext1 = testExtractor(value, context1)
    val ext2 = testExtractor(value, context2, finalParameters)
    val extractor = excluding(false, ext1, ext2) { optDefined =>
      optDefined should be(None)
      optDefined
    }

    val (result, ctx) = ParameterExtractor.runExtractor(extractor, context1)
    ctx.parameters should be(finalParameters)
    result should be(Success(None))
  }

  it should "provide an extractor defining excluding options that returns the single defined value" in {
    val context1 = extractionContext()
    val context2 = extractionContext(NextParameters, reader = context1.reader)
    val finalParameters = generateParameters(2)
    val valueNone: SingleOptionValue[String] = Success(None)
    val valueDefined: SingleOptionValue[String] = Success(Some("definedValue"))
    val ext1 = testExtractor(valueNone, context1)
    val ext2 = testExtractor(valueDefined, context2, finalParameters)
    val extractor = excluding(false, ext1, ext2)(_.map(_._2))

    val (result, ctx) = ParameterExtractor.runExtractor(extractor, context1)
    ctx.parameters should be(finalParameters)
    result should be(valueDefined)
  }

  it should "provide an extractor defining excluding options that handles failures in the child extractors" in {
    val nextParams2 = generateParameters(2)
    val nextParams3 = generateParameters(3)
    val context1 = extractionContext()
    val context2 = extractionContext(NextParameters, reader = context1.reader)
    val context3 = extractionContext(nextParams2, reader = context1.reader)
    val failure1 = ExtractionFailure(TestParamKey, new IllegalArgumentException("Test exception 1"), None, context1)
    val failure2 = ExtractionFailure(ParameterKey("k3", shortAlias = false),
      new IllegalStateException("Test exception 2"), None, context3)
    val exception1 = ParameterExtractionException(failure1)
    val exception2 = ParameterExtractionException(failure2)
    val valueErr1: SingleOptionValue[String] = Failure(exception1)
    val valueErr2: SingleOptionValue[String] = Failure(exception2)
    val valueDefined: SingleOptionValue[String] = Success(Some("successful and defined"))
    val ext1 = testExtractor(valueErr1, context1)
    val ext2 = testExtractor(valueDefined, context2, nextParams2, key = ParameterKey("k2", shortAlias = false))
    val ext3 = testExtractor(valueErr2, context3, nextParams3, key = failure2.key)
    val extractor = excluding(false, ext1, ext2, ext3)(_.map(_._2))

    val exception = expectExtractionException(ParameterExtractor.tryExtractor(extractor, context1))
    exception.failures should contain only(failure1, failure2)
  }

  it should "provide an extractor defining excluding options that detects multiple defined options" in {
    val nextParams2 = generateParameters(2)
    val nextParams3 = generateParameters(3)
    val nextParams4 = generateParameters(4)
    val context1 = extractionContext()
    val context2 = extractionContext(NextParameters, reader = context1.reader)
    val context3 = extractionContext(nextParams2, reader = context1.reader)
    val context4 = extractionContext(nextParams3, reader = context1.reader)
    val valueUndefined: SingleOptionValue[String] = Success(None)
    val valueDefined1: SingleOptionValue[String] = Success(Some("v1"))
    val valueDefined2: SingleOptionValue[String] = Success(Some("v2"))
    val valueDefined3: SingleOptionValue[String] = Success(Some("v3"))
    val ext1 = testExtractor(valueUndefined, context1)
    val ext2 = testExtractor(valueDefined1, context2, nextParams2, key = ParameterKey("k2", shortAlias = false))
    val ext3 = testExtractor(valueDefined2, context3, nextParams3, key = ParameterKey("k3", shortAlias = false))
    val ext4 = testExtractor(valueDefined3, context4, nextParams4, key = nextParams4.parametersMap.head._1)

    val mappedKeys = collection.mutable.Set.empty[ParameterKey]
    val extractor = excluding(false, ext1, ext2, ext3, ext4) {
      case Some(value) =>
        mappedKeys += value._1
        value._2
      case None => fail("Unexpected undefined option.")
    }

    val exception = expectExtractionException(ParameterExtractor.tryExtractor(extractor, context1))
    exception.failures should have size 1
    checkExtractionFailure[IllegalArgumentException](exception.failures.head, expKey = ext4.key)(ext2.key.key,
      ext3.key.key)
    exception.failures.head.optElement should be(nextParams4.parametersMap.get(ext4.key).map(_.head))
  }

  it should "provide an extractor defining excluding options that allows overriding option values" in {
    val Key2 = pk("k2")
    val params = Map(TestParamKey -> List("someOtherResult"),
      Key2 -> List("ResultValue"))
    val context = extractionContext(Parameters(toParamValues(params), Set.empty))
    val otherValue: SingleOptionValue[String] = Success(Some("aValue"))
    val resultValue: SingleOptionValue[String] = Success(Some("ResultValue"))
    val ext1 = testExtractor(otherValue, context, context.parameters)
    val ext2 = testExtractor(resultValue, context, context.parameters, Key2)
    val extractor = excluding(true, ext1, ext2)(_.map(_._2))

    val (result, _) = ParameterExtractor.runExtractor(extractor, context)
    result should be(resultValue)
  }

  it should "provide an extractor defining excluding options that allows overriding by the last option value" in {
    val MappedResult = "The mapped result"
    val Key2 = pk("k2")
    val Key3 = pk("k3")
    val params = Map(TestParamKey -> List("someOtherResult"),
      Key2 -> List("stillAnotherResult"),
      Key3 -> List("The real result"))
    val context = extractionContext(Parameters(toParamValues(params), Set.empty))
    val otherValue: SingleOptionValue[String] = Success(Some("aValue"))
    val resultValue: SingleOptionValue[String] = Success(Some("ResultValue"))
    val ext1 = testExtractor(otherValue, context, context.parameters)
    val ext2 = testExtractor(otherValue, context, context.parameters, Key2)
    val ext3 = testExtractor(resultValue, context, context.parameters, Key3)
    val extractor = excluding(true, ext3, ext1, ext2) { opt =>
      opt.map { kv =>
        if (kv._2 == "ResultValue") MappedResult else "BOOM"
      }
    }

    val (result, _) = ParameterExtractor.runExtractor(extractor, context)
    result should be(Success(Some(MappedResult)))
  }

  it should "provide an extractor defining excluding options that handles undefined values in override mode" in {
    val nextParams2 = generateParameters(2)
    val context1 = extractionContext()
    val context2 = extractionContext(NextParameters, reader = context1.reader)
    val definedValue: SingleOptionValue[String] = Success(Some("aValue"))
    val ext1 = testExtractor(definedValue, context1)
    val ext2 = testExtractor(definedValue, context2, nextParams2, key = ParameterKey("k2", shortAlias = false))
    val extractor = excluding(true, ext1, ext2)(_.map(_._2))

    expectExtractionException(ParameterExtractor.tryExtractor(extractor, context1))
  }

  it should "provide an extractor defining excluding options that sorts by all values in override mode" in {
    val Key2 = pk("k2")
    val Key3 = pk("k3")
    val params = Map(TestParamKey -> List(OptionElement(TestParamKey, Some("r1"), 5)),
      Key2 -> List(),
      Key3 -> List(OptionElement(Key3, Some("r3.1"), 1), OptionElement(Key3, Some("r.2"), 6)))
    val context = extractionContext(Parameters(params, Set.empty))
    val otherValue: SingleOptionValue[String] = Success(Some("aValue"))
    val resultValue: SingleOptionValue[String] = Success(Some("ResultValue"))
    val ext1 = testExtractor(otherValue, context, context.parameters)
    val ext2 = testExtractor(otherValue, context, context.parameters, Key2)
    val ext3 = testExtractor(resultValue, context, context.parameters, Key3)
    val extractor = excluding(true, ext1, ext3, ext2)(_.map(_._2))

    val (result, _) = ParameterExtractor.runExtractor(extractor, context)
    result should be(resultValue)
  }

  it should "provide an extractor for excluding switches that returns None if all switches are unset" in {
    val context1 = extractionContext()
    val context2 = extractionContext(NextParameters, reader = context1.reader)
    val finalParameters = generateParameters(2)
    val ext1 = testExtractor(Try(false), context1)
    val ext2 = testExtractor(Try(false), context2, finalParameters)
    val extractor = excludingSwitches(false, ext1, ext2)

    val (result, ctx) = ParameterExtractor.runExtractor(extractor, context1)
    ctx.parameters should be(finalParameters)
    result should be(Success(None))
  }

  it should "provide an extractor for excluding switches that returns the name of the selected switch" in {
    val SelectedKey = "selected"
    val context1 = extractionContext()
    val context2 = extractionContext(NextParameters, reader = context1.reader)
    val ext1 = testExtractor(Try(false), context1)
    val ext2 = testExtractor(Try(true), context2, generateParameters(2),
      ParameterKey(SelectedKey, shortAlias = false))
    val extractor = excludingSwitches(false, ext1, ext2)

    val (result, _) = ParameterExtractor.runExtractor(extractor, context1)
    result should be(Success(Some(SelectedKey)))
  }

  it should "provide an extractor for excluding switches that handles a failure in the switches" in {
    val context1 = extractionContext()
    val context2 = extractionContext(NextParameters, reader = context1.reader)
    val failure = ExtractionFailure(ParameterKey("k3", shortAlias = false),
      new IllegalStateException("Test exception 2"), None, context1)
    val switchException = ParameterExtractionException(failure)
    val ext1 = testExtractor(Try(false), context1)
    val ext2 = testExtractor(Failure(switchException): Try[Boolean], context2, generateParameters(2),
      ParameterKey("k2", shortAlias = false))
    val extractor = excludingSwitches(false, ext1, ext2)

    val exception = expectExtractionException(ParameterExtractor.tryExtractor(extractor, context1))
    exception.failures should contain only failure
  }

  it should "provide an extractor for excluding switches that detects multiple set switches" in {
    val context1 = extractionContext()
    val context2 = extractionContext(NextParameters, reader = context1.reader)
    val ext1 = testExtractor(Try(true), context1)
    val ext2 = testExtractor(Try(true), context2, generateParameters(2), ParameterKey("k2", shortAlias = false))
    val extractor = excludingSwitches(false, ext1, ext2)

    val exception = expectExtractionException(ParameterExtractor.tryExtractor(extractor, context1))
    exception.failures should have size 1
    checkExtractionFailure[IllegalArgumentException](exception.failures.head, expKey = ext2.key)(TestParamKey.key)
  }

  it should "provide an extractor for excluding switches that allows overriding" in {
    val Key2 = pk("k2")
    val params = Parameters(toParamValues(Map(TestParamKey -> List("true"), Key2 -> List("true"))), Set.empty)
    val context = extractionContext(params)
    val ext1 = testExtractor(Try(true), context, params)
    val ext2 = testExtractor(Try(true), context, params, key = Key2)
    val extractor = excludingSwitches(true, ext1, ext2)

    val (result, _) = ParameterExtractor.runExtractor(extractor, context)
    result should be(Success(Some(Key2.key)))
  }

  it should "check whether all parameters have been consumed" in {
    val Key2 = pk("otherKey1")
    val Key3 = pk("otherKey2")
    val parameters = Parameters(TestParameters.parametersMap ++
      toParamValues(Map(Key2 -> List("v1", "v2"), Key3 -> List("v3"))), Set(TestParamKey, Key2, Key3))
    val context = TestContext.copy(parameters = parameters)

    val validatedContext = ParameterExtractor.checkParametersConsumed(context)
    validatedContext should be(Success(context))
  }

  it should "detect parameters that have not been consumed" in {
    val Key2 = pk("otherKey1")
    val Key3 = pk("otherKey2")
    val parameters = Parameters(TestParameters.parametersMap ++
      toParamValues(Map(Key2 -> List("v1", "v2"), Key3 -> List("v3"))), Set(TestParamKey))
    val context = TestContext.copy(parameters = parameters)

    val validatedContext = ParameterExtractor.checkParametersConsumed(context)
    validatedContext match {
      case Failure(exception: ParameterExtractionException) =>
        exception.failures should have size 2
        exception.failures.map(_.key) should contain only(Key2, Key3)
        exception.failures.forall(_.context == context) shouldBe true
        exception.failures.foreach { failure =>
          failure.optElement should be(None)
          failure.cause shouldBe a[IllegalArgumentException]
          failure.cause.getMessage should include(exceptionMessage(failure.key,
            FailureCodes.UnsupportedParameter, Seq.empty))
        }
      case r => fail("Unexpected result: " + r)
    }
  }
}
