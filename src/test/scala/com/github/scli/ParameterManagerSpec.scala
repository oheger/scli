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

import com.github.scli.ParameterExtractor.{CliExtractor, ParameterExtractionException}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.{Failure, Success, Try}

object ParameterManagerSpec {
  /** A key for a test option. */
  private val TestOptionKey = "theTestOption"

  /** Test value of the test option. */
  private val TestOptionValue = "testValue"

  /** A help text for the test option. */
  private val HelpTestOption = "This is the help text for the test option."

  /**
   * A test extractor that can be used by test cases. The extractor returns
   * the current parameters map as result. It also adds the test option to the
   * map of accessed options and to the model context.
   */
  private val TestExtractor = ParameterManager.wrapTryExtractor(CliExtractor(context => {
    if (context.reader == DefaultConsoleReader) {
      val nextContext = context.update(context.parameters.keyAccessed(TestOptionKey),
        context.helpContext.addOption(TestOptionKey, Some(HelpTestOption)))
      (context.parameters.parametersMap, nextContext)
    } else (Map.empty, context)
  }))

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
    val ExpParamsMap = Map(TestOptionKey -> List(TestOptionValue),
      "foo" -> List("bar", "baz"))

    import scala.language.existentials
    val (res, context) = triedResult(ParameterManager.processCommandLine(args, TestExtractor,
      checkUnconsumedParameters = false))
    res should be(ExpParamsMap)
    context.parameters.parametersMap should be(ExpParamsMap)
    context.parameters.accessedParameters should contain only TestOptionKey
    context.helpContext.options(TestOptionKey).attributes(ParameterModel.AttrHelpText) should be(HelpTestOption)
  }

  it should "check for unconsumed parameters" in {
    val args = List("--" + TestOptionKey, TestOptionValue, "--foo", "value")

    val exception = failedResult(ParameterManager.processCommandLine(args, TestExtractor))
    exception.failures should have size 1
    exception.failures.head.key should be("foo")
  }

  it should "handle a failed extractor" in {
    val args = List("--" + TestOptionKey, TestOptionValue)
    val extractor = ParameterExtractor.multiOptionValue(TestOptionKey)
      .toInt

    val exception = failedResult(ParameterManager.processCommandLine(args, extractor))
    exception.failures should have size 1
    exception.failures.head.key should be(TestOptionKey)
    exception.failures.head.message should include("NumberFormatException")
    exception.parameterContext.parameters.parametersMap.keys should contain(TestOptionKey)
    val helpContext = exception.parameterContext.helpContext
    helpContext.options.keys should contain(TestOptionKey)
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
    exception.failures.map(_.key) should contain only(TestOptionKey, "missing", "unknownOption")
  }

  it should "add all failures to the model context in the resulting exception" in {
    val args = List("--" + TestOptionKey, TestOptionValue, "--unknownOption", "shouldFail")
    val extractor = ParameterExtractor.multiOptionValue(TestOptionKey).toInt

    val exception = failedResult(ParameterManager.processCommandLine(args, extractor))
    val helpContext = exception.parameterContext.helpContext
    helpContext.options(TestOptionKey).attributes.keys should contain(ParameterModel.AttrErrorMessage)
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

  it should "support customizing the parsing function" in {
    val args = List("/" + TestOptionKey, TestOptionValue)
    val key = TestOptionKey.toLowerCase(Locale.ROOT)
    val extractor = ParameterExtractor.optionValue(key).mandatory
    val prefixes = ParameterParser.OptionPrefixes("/")
    val keyExtractor = ParameterManager.defaultKeyExtractor(prefixes) andThen (opt => opt.map(_.toLowerCase(Locale.ROOT)))
    val classifierFunc =
      ParameterParser.classifierOf(ParameterManager.defaultExtractedKeyClassifiers(extractor): _*)(keyExtractor)

    val parseFunc = ParameterManager.parsingFunc(extractor, classifierFunc)
    val (res, _) = triedResult(ParameterManager.processCommandLine(args, extractor, parser = parseFunc))
    res should be(TestOptionValue)
  }

  it should "support loading parameter files and handle corresponding exceptions" in {
    val FileOption = "file"
    val FileName = "someFile.txt"
    val args = List("--" + TestOptionKey, TestOptionValue, "--" + FileOption, FileName)
    val extractor = ParameterExtractor.multiOptionValue(TestOptionKey, help = Some(HelpTestOption))

    val parseFunc = ParameterManager.parsingFunc(extractor, optFileOption = Some(FileOption))
    val exception = failedResult(ParameterManager.processCommandLine(args, extractor, parseFunc,
      checkUnconsumedParameters = false))
    exception.failures should have size 1
    val failure = exception.failures.head
    failure.key should be(FileOption)
    failure.message should include(FileName)
    val context = exception.parameterContext
    context.parameters.parametersMap.keys should contain(TestOptionKey)
    val helpContext = context.helpContext
    val testOptionAttrs = helpContext.options(TestOptionKey)
    testOptionAttrs.attributes(ParameterModel.AttrHelpText) should be(HelpTestOption)
    val fileOptionAttrs = helpContext.options(FileOption)
    fileOptionAttrs.attributes.keys should contain(ParameterModel.AttrErrorMessage)
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
}
