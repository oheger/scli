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

import java.util.Locale
import java.util.concurrent.atomic.AtomicInteger

import com.github.scli.ParameterExtractor.{CliExtractor, ExtractionContext, ExtractionFailure, FailureCodes, ParameterExtractionException, Parameters}
import com.github.scli.ParameterManager.{ExtractionSpec, ProcessingContext, ProcessingResult}
import com.github.scli.ParameterModel.{AliasMapping, AttrErrCause, ModelContext, ParameterKey}
import com.github.scli.ParameterParser.{CliElement, ParameterFileException}
import com.github.scli.ParametersTestHelper._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.SortedSet
import scala.language.existentials
import scala.util.{Failure, Success, Try}

object ParameterManagerSpec {
  /** A key for a test option. */
  private val TestOptionKey = "theTestOption"

  /** The ''ParameterKey'' for the test option. */
  private val TestOptionPk = pk(TestOptionKey)

  /** Test value of the test option. */
  private val TestOptionValue = "testValue"

  /** A help text for the test option. */
  private val HelpTestOption = "This is the help text for the test option."

  /** The default test extractor. */
  private val TestExtractor = createTestExtractor(DefaultConsoleReader)

  /**
   * Generates an extractor based on the given one that counts the number of
   * its invocations. This can be used to test how often the model context is
   * created.
   *
   * @param extractor the base extractor
   * @tparam A the result type of the extractor
   * @return a tuple with the counting extractor and the counter it uses
   */
  private def countingExtractor[A](extractor: CliExtractor[A]): (CliExtractor[A], AtomicInteger) = {
    val counter = new AtomicInteger
    val extCount = extractor.map { result =>
      counter.incrementAndGet()
      result
    }
    (extCount, counter)
  }

  /**
   * Creates a test extraction context that does not contain any meaningful
   * values.
   *
   * @return the test extraction context
   */
  private def createExtractionContext(): ExtractionContext = {
    val modelContext = new ModelContext(Map.empty, SortedSet.empty, AliasMapping(Map.empty, Map.empty),
      None, List("some", "groups"))
    val params = Parameters(toParamValues(Map(TestOptionPk -> List(TestOptionValue))), Set.empty)
    ExtractionContext(params, modelContext, DummyConsoleReader)
  }

  /**
   * Creates a test extractor that can be used by test cases. The extractor
   * returns the current parameters map as result if the expected reader is
   * found in the extraction context. It also adds the test option to the map of
   * accessed options and to the model context.
   *
   * @param expReader the expected console reader
   * @return the test extractor
   */
  private def createTestExtractor(expReader: ConsoleReader):
  CliExtractor[Try[Map[ParameterKey, Iterable[CliElement]]]] =
    ParameterManager.wrapTryExtractor(CliExtractor(context => {
      if (context.reader == expReader) {
        val nextContext = context.update(context.parameters.keyAccessed(TestOptionPk),
          context.modelContext.addOption(TestOptionPk, Some(HelpTestOption)))
        (context.parameters.parametersMap, nextContext)
      } else (Map.empty, context)
    }))
}

/**
 * Test class for ''ParameterManager''.
 */
class ParameterManagerSpec extends AnyFlatSpec with Matchers {

  import ParameterManagerSpec._

  /**
   * Checks whether the given ''Try'' has a success result and returns it.
   *
   * @param triedRes the tried result
   * @tparam A the type of the result
   * @return the success result
   */
  private def triedResult[A](triedRes: Try[A]): A =
    triedRes match {
      case Success(value) => value
      case r => fail("Unexpected result: " + r)
    }

  /**
   * Checks whether the given ''Try'' has failed with a parameter extraction
   * exception and returns it.
   *
   * @param triedRes the tried result
   * @return the exception that has been extracted
   */
  private def failedResult(triedRes: Try[_]): ParameterExtractionException =
    triedRes match {
      case Failure(exception: ParameterExtractionException) => exception
      case r => fail("Unexpected result: " + r)
    }

  "ParameterManager" should "process a simple command line with defaults" in {
    val args = List("--" + TestOptionKey, TestOptionValue, "--foo", "bar", "--foo", "baz")
    val ExpParamsMap = toParamValues(Map(TestOptionPk -> List(TestOptionValue),
      pk("foo") -> List("bar", "baz")))

    val (res, procContext) = triedResult(ParameterManager.processCommandLine(args, TestExtractor,
      checkUnconsumedParameters = false))
    res should be(ExpParamsMap)
    val context = procContext.parameterContext
    context.parameters.parametersMap should be(ExpParamsMap)
    context.parameters.accessedParameters should contain only TestOptionPk
    context.modelContext.options(TestOptionPk).attributes(ParameterModel.AttrHelpText) should be(HelpTestOption)
    procContext.helpRequested shouldBe false
  }

  it should "check for unconsumed parameters" in {
    val args = List("--" + TestOptionKey, TestOptionValue, "--foo", "value")

    val exception = failedResult(ParameterManager.processCommandLine(args, TestExtractor))
    exception.failures should have size 1
    exception.failures.head.key should be(pk("foo"))
  }

  it should "check for unconsumed parameters at the end of the command line" in {
    val args = List("--" + TestOptionKey, TestOptionValue, "--foo")

    val exception = failedResult(ParameterManager.processCommandLine(args, TestExtractor))
    exception.failures should have size 1
    exception.failures.head.key should be(pk("foo"))
  }

  it should "handle a failed extractor" in {
    val args = List("--" + TestOptionKey, TestOptionValue)
    val extractor = ParameterExtractor.multiOptionValue(TestOptionKey)
      .toInt

    val exception = failedResult(ParameterManager.processCommandLine(args, extractor))
    exception.failures should have size 1
    exception.failures.head.key should be(TestOptionPk)
    exception.failures.head.cause shouldBe a[NumberFormatException]
    exception.extractionContext.parameters.parametersMap.keys should contain(TestOptionPk)
    val modelContext = exception.extractionContext.modelContext
    modelContext.options.keys should contain(TestOptionPk)
  }

  it should "combine failures of the extractor with failures for unconsumed parameters" in {
    val args = List("--" + TestOptionKey, TestOptionValue, "--unknownOption", "shouldFail")
    val ext1 = ParameterExtractor.multiOptionValue(TestOptionKey).toInt
    val ext2 = ParameterExtractor.optionValue("missing").mandatory
    val extractor = for {
      v1 <- ext1
      v2 <- ext2
    } yield ParameterExtractor.createRepresentation(v1, v2) {
      (_, _)
    }

    val exception = failedResult(ParameterManager.processCommandLine(args, extractor))
    exception.failures.map(_.key) should contain only(TestOptionPk, pk("missing"), pk("unknownOption"))
  }

  it should "support options and switches per default classifier function" in {
    val KeySwitch = "flag"
    val args = List("--" + TestOptionKey, TestOptionValue, "--" + KeySwitch)
    val extOpt = ParameterExtractor.optionValue(TestOptionKey).mandatory
    val extSwitch = ParameterExtractor.switchValue(KeySwitch)
    val extractor = for {
      opt <- extOpt
      flag <- extSwitch
    } yield (opt, flag)

    val (res, _) = triedResult(ParameterManager.processCommandLine(args, ParameterManager.wrapTryExtractor(extractor)))
    res should be((Success(TestOptionValue), Success(true)))
  }

  it should "support different option prefixes" in {
    val args = List("/" + TestOptionKey, TestOptionValue)
    val extractor = ParameterExtractor.optionValue(TestOptionKey).mandatory
    val prefixes = ParameterParser.OptionPrefixes(pk("/"))
    val spec = ExtractionSpec(extractor, prefixes = prefixes)

    val (res, _) = triedResult(ParameterManager.processCommandLineSpec(args, spec))
    res should be(TestOptionValue)
  }

  it should "support advanced customization of the parsing function" in {
    val args = List("--" + TestOptionKey, TestOptionValue)
    val key = TestOptionKey.toLowerCase(Locale.ROOT)
    val extractor = ParameterExtractor.optionValue(key).mandatory
    val keyExtractor = ParameterManager.defaultKeyExtractor() andThen (opt =>
      opt.map(key => key.copy(key = key.key.toLowerCase(Locale.ROOT))))
    val spec = ExtractionSpec(extractor, keyExtractor = keyExtractor)

    val (res, _) = triedResult(ParameterManager.processCommandLineSpec(args, spec))
    res should be(TestOptionValue)
  }

  it should "not construct a model context if this is not needed" in {
    val extractor = ParameterExtractor.inputValue(0, last = true)
    val args = List("input")
    val (extCnt, counter) = countingExtractor(extractor)

    val (res, _) = triedResult(ParameterManager.processCommandLine(args, extCnt))
    res should be(Some(args.head))
    counter.get() should be(1)
  }

  it should "construct the model context at most once" in {
    val KeySwitch = "flag"
    val args = List("--" + TestOptionKey, TestOptionValue, "--" + KeySwitch)
    val extOpt = ParameterExtractor.optionValue(TestOptionKey).mandatory
    val extSwitch = ParameterExtractor.switchValue(KeySwitch)
    val extractor = for {
      opt <- extOpt
      flag <- extSwitch
    } yield (opt, flag)
    val (extCnt, counter) = countingExtractor(extractor)

    triedResult(ParameterManager.processCommandLine(args, ParameterManager.wrapTryExtractor(extCnt)))
    counter.get() should be(2)
  }

  it should "handle aliases" in {
    val AliasShort = "o"
    val AliasLong = "alternative-option-key"
    val AliasValue = "anotherValue"
    val args = List("-" + AliasShort, TestOptionValue, "--" + AliasLong, AliasValue)
    val extractor = ParameterExtractor.multiOptionValue(TestOptionKey)
      .alias(AliasShort)
      .alias(AliasLong, shortAlias = false)

    val (res, _) = triedResult(ParameterManager.processCommandLine(args, extractor))
    res.toList should contain theSameElementsInOrderAs List(TestOptionValue, AliasValue)
  }

  it should "detect an alias using an incorrect prefix" in {
    val Alias = "o"
    val args = List("--" + Alias, TestOptionValue)
    val extractor = ParameterExtractor.withAlias(ParameterExtractor.optionValue(TestOptionKey), Alias)

    val exception = failedResult(ParameterManager.processCommandLine(args, extractor))
    exception.failures should have size 1
    exception.failures.head.key should be(ParameterKey(Alias, shortAlias = false))
  }

  it should "support combined single-letter switches" in {
    val args = List("-dv")
    val extVerbose = ParameterExtractor.switchValue("verbose").alias("v")
    val extDebug = ParameterExtractor.switchValue("debug").alias("d")
    val extractor = for {
      verbose <- extVerbose
      debug <- extDebug
    } yield (verbose, debug)
    val spec = ExtractionSpec(ParameterManager.wrapTryExtractor(extractor), supportCombinedSwitches = true)

    val (res, _) = triedResult(ParameterManager.processCommandLineSpec(args, spec))
    res should be((Success(true), Success(true)))
  }

  it should "disable combined single-letter switches per default" in {
    val args = List("-dv")
    val extVerbose = ParameterExtractor.switchValue("verbose").alias("v")
    val extDebug = ParameterExtractor.switchValue("debug").alias("d")
    val extractor = for {
      verbose <- extVerbose
      debug <- extDebug
    } yield (verbose, debug)
    val extCtx = ExtractionSpec(ParameterManager.wrapTryExtractor(extractor))

    val exception = failedResult(ParameterManager.processCommandLineSpec(args, extCtx))
    exception.failures should have size 1
    exception.failures.head.key should be(ParameterKey("dv", shortAlias = true))
  }

  it should "support loading parameter files and handle corresponding exceptions" in {
    val FileOption = pk("file")
    val FileName = "someFile.txt"
    val args = List("--" + TestOptionKey, TestOptionValue, "--" + FileOption.key, FileName)
    val extractor = ParameterExtractor.optionValue(TestOptionKey, help = Some(HelpTestOption))
    val spec = ExtractionSpec(extractor, fileOptions = List(FileOption))

    val classifierFunc = ParameterManager.classifierFunc(spec)
    val exception = failedResult(ParameterManager.processParameterFiles(args, spec)(classifierFunc))
    exception.failures should have size 1
    val failure = exception.failures.head
    failure.key should be(FileOption)
    failure.cause shouldBe a[ParameterFileException]
    failure.cause.getMessage should include(FileName)
    val context = exception.extractionContext
    val modelContext = context.modelContext
    val testOptionAttrs = modelContext.options(TestOptionPk)
    testOptionAttrs.attributes(ParameterModel.AttrHelpText) should be(HelpTestOption)
  }

  it should "detect a file option with a short alias if combined switches are enabled" in {
    val FileOption = ParameterKey("f", shortAlias = true)
    val args = List("--" + TestOptionKey, TestOptionValue, "-" + FileOption.key, "nonExisting.txt")
    val extractor = ParameterExtractor.optionValue(TestOptionKey, help = Some(HelpTestOption))
    val spec = ExtractionSpec(extractor, fileOptions = List(FileOption), supportCombinedSwitches = true)

    val classifierFunc = ParameterManager.classifierFunc(spec)
    val exception = failedResult(ParameterManager.processParameterFiles(args, spec)(classifierFunc))
    exception.failures should have size 1
    val failure = exception.failures.head
    failure.key should be(FileOption)
  }

  it should "detect a help switch on the command line" in {
    val HelpKey = ParameterKey("help", shortAlias = false)
    val args = List("--" + TestOptionKey, TestOptionValue, "--" + HelpKey.key, "--foo", "bar", "--foo", "baz")
    val ExpParamsMap = toParamValues(Map(TestOptionPk -> List(TestOptionValue),
      pk("foo") -> List("bar", "baz"),
      HelpKey -> List("true")))
    val helpExtractor = ParameterExtractor.switchValue(HelpKey.key)
    val spec = ExtractionSpec(createTestExtractor(DummyConsoleReader), optHelpExtractor = Some(helpExtractor))

    val (res, procContext) = triedResult(ParameterManager.processCommandLineSpec(args, spec,
      checkUnconsumedParameters = false))
    res should contain theSameElementsAs ExpParamsMap
    val context = procContext.parameterContext
    context.parameters.parametersMap should be(ExpParamsMap)
    context.parameters.accessedParameters should contain only(TestOptionPk, HelpKey)
    context.modelContext.options(TestOptionPk).attributes(ParameterModel.AttrHelpText) should be(HelpTestOption)
    procContext.helpRequested shouldBe true
  }

  it should "handle a failure when extracting the help flag" in {
    val HelpKey = ParameterKey("please-help", shortAlias = false)
    val Alias = "h"
    val args = List("--" + TestOptionKey, TestOptionValue, "-" + Alias, "maybe", "--foo", "bar")
    val helpExtractor = ParameterExtractor.optionValue(HelpKey.key)
      .alias(Alias)
      .toBoolean
      .mandatory
    val spec = ExtractionSpec(createTestExtractor(DummyConsoleReader), optHelpExtractor = Some(helpExtractor))

    val exception = failedResult(ParameterManager.processCommandLineSpec(args, spec,
      checkUnconsumedParameters = false))
    exception.failures should have size 1
    exception.failures.head.key should be(HelpKey)
  }

  it should "evaluate a successful processing result" in {
    val ExtractResult = 42
    val context = ProcessingContext(createExtractionContext(), helpRequested = false, optFailureContext = None)
    val processResult: ProcessingResult[Int] = Success((ExtractResult, context))

    ParameterManager.evaluate(processResult) should be(Right(ExtractResult))
  }

  it should "evaluate a successful processing result if help was requested" in {
    val context = ProcessingContext(createExtractionContext(), helpRequested = true, optFailureContext = None)
    val processResult: ProcessingResult[String] = Success(("ignored", context))

    ParameterManager.evaluate(processResult) should be(Left(context))
  }

  it should "evaluate a failed processing result" in {
    val extrCtx = createExtractionContext()
    val cause = new Exception("failure")
    val failure = ExtractionFailure(TestOptionPk, cause, None, extrCtx)
    val exception = ParameterExtractionException(failure)
    val processResult: ProcessingResult[String] = Failure(exception)

    ParameterManager.evaluate(processResult) match {
      case Left(context) =>
        context.parameterContext should be(extrCtx)
        context.helpRequested shouldBe false
        context.optFailureContext.isDefined shouldBe true
        val failureContext = context.optFailureContext.get
        failureContext.failures should have size 1
        val data = failureContext.parameterMetaData.head
        data.key should be(TestOptionPk)
        data.attributes.get(AttrErrCause) should be(Some(cause))
      case r => fail("Unexpected result: " + r)
    }
  }

  it should "evaluate a failed processing result with an unexpected exception" in {
    val exception = new IllegalStateException("Unexpected exception")
    val processResult: ProcessingResult[String] = Failure(exception)

    val thrown = intercept[IllegalArgumentException] {
      ParameterManager.evaluate(processResult)
    }
    thrown.getCause should be(exception)
  }

  /**
   * Checks an exception produced by the default exception generator function.
   *
   * @param ex     the exception to check
   * @param expMsg the expected exception message
   */
  private def checkFailureException(ex: Throwable, expMsg: String): Unit = {
    ex shouldBe a[IllegalArgumentException]
    ex.getMessage should be(expMsg)
  }

  /**
   * Helper function to check the exception generated for a specific failure
   * code by the default exception generator function.
   *
   * @param code   the failure code
   * @param params additional parameters
   * @param expMsg the expected message
   */
  private def checkExceptionForCode(code: FailureCodes.Value, params: Seq[String], expMsg: String): Unit = {
    val exFunc = ParameterManager.defaultExceptionGenerator
    val ex = exFunc(TestOptionPk, code, params)
    checkFailureException(ex, expMsg)
  }

  it should "handle TooManyInputParameters in the default exception generator function" in {
    checkExceptionForCode(FailureCodes.TooManyInputParameters, Seq("5"),
      "Too many input arguments; expected at most 5")
  }

  it should "handle UnexpectedGroup in the default exception generator function" in {
    val GroupValue = "unknownGroup"
    val KnownGroups = "[foo, bar, baz]"

    checkExceptionForCode(FailureCodes.UnknownGroup, Seq(GroupValue, KnownGroups),
      "Invalid value \"" + GroupValue + "\". Expected one of " + KnownGroups)
  }

  it should "handle MultipleValues in the default exception generator function" in {
    val Values = "[foo, bar, baz]"

    checkExceptionForCode(FailureCodes.MultipleValues, Seq(Values),
      "Single value expected, but got " + Values)
  }

  it should "handle MandatoryParameterMissing in the default exception generator function" in {
    checkExceptionForCode(FailureCodes.MandatoryParameterMissing, Seq.empty,
      "Mandatory parameter has no value")
  }

  it should "handle MultiplicityTooLow in the default exception generator function" in {
    checkExceptionForCode(FailureCodes.MultiplicityTooLow, Seq("2"),
      "Too few values; parameter must have at least 2 values")
  }

  it should "handle MultiplicityTooHigh in the default exception generator function" in {
    checkExceptionForCode(FailureCodes.MultiplicityTooHigh, Seq("5"),
      "Too many values; parameter must have at most 5 values")
  }

  it should "handle UnsupportedParameter in the default exception generator function" in {
    checkExceptionForCode(FailureCodes.UnsupportedParameter, Seq.empty,
      "Unsupported parameter")
  }

  it should "support overriding default exception messages" in {
    val Message = "I do not know this parameter?!"
    val Messages = Map(FailureCodes.UnsupportedParameter -> Message)

    val generator = ParameterManager.exceptionGenerator(Messages)
    val ex = generator(TestOptionPk, FailureCodes.UnsupportedParameter, Seq.empty)
    ex.getMessage should be(Message)
  }

  it should "use default exception messages for codes that are not defined" in {
    val generator = ParameterManager.exceptionGenerator(Map.empty)

    val ex = generator(TestOptionPk, FailureCodes.UnsupportedParameter, Seq.empty)
    ex.getMessage should be("Unsupported parameter")
  }
}
