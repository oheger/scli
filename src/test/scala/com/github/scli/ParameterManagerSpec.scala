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
import com.github.scli.ParameterManager.ExtractionSpec
import com.github.scli.ParameterModel.ParameterKey
import com.github.scli.ParametersTestHelper._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

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

  /**
   * A test extractor that can be used by test cases. The extractor returns
   * the current parameters map as result. It also adds the test option to the
   * map of accessed options and to the model context.
   */
  private val TestExtractor = ParameterManager.wrapTryExtractor(CliExtractor(context => {
    if (context.reader == DefaultConsoleReader) {
      val nextContext = context.update(context.parameters.keyAccessed(TestOptionPk),
        context.modelContext.addOption(TestOptionPk, Some(HelpTestOption)))
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
    val ExpParamsMap = Map(TestOptionPk -> List(TestOptionValue),
      pk("foo") -> List("bar", "baz"))

    import scala.language.existentials
    val (res, context) = triedResult(ParameterManager.processCommandLine(args, TestExtractor,
      checkUnconsumedParameters = false))
    res should be(ExpParamsMap)
    context.parameters.parametersMap should be(ExpParamsMap)
    context.parameters.accessedParameters should contain only TestOptionPk
    context.modelContext.options(TestOptionPk).attributes(ParameterModel.AttrHelpText) should be(HelpTestOption)
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
    exception.failures.head.message should include("NumberFormatException")
    exception.parameterContext.parameters.parametersMap.keys should contain(TestOptionPk)
    val modelContext = exception.parameterContext.modelContext
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

  it should "add all failures to the model context in the resulting exception" in {
    val args = List("--" + TestOptionKey, TestOptionValue, "--unknownOption", "shouldFail")
    val extractor = ParameterExtractor.multiOptionValue(TestOptionKey).toInt

    val exception = failedResult(ParameterManager.processCommandLine(args, extractor))
    val modelContext = exception.parameterContext.modelContext
    modelContext.options(TestOptionPk).attributes.keys should contain(ParameterModel.AttrErrorMessage)
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
    failure.message should include(FileName)
    val context = exception.parameterContext
    val modelContext = context.modelContext
    val testOptionAttrs = modelContext.options(TestOptionPk)
    testOptionAttrs.attributes(ParameterModel.AttrHelpText) should be(HelpTestOption)
    val fileOptionAttrs = modelContext.options(FileOption)
    fileOptionAttrs.attributes.keys should contain(ParameterModel.AttrErrorMessage)
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
}
