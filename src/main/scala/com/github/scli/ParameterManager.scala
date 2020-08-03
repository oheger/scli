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

import com.github.scli.ParameterExtractor._
import com.github.scli.ParameterModel.{ModelContext, ParameterKey}
import com.github.scli.ParameterParser._

import scala.util.{Failure, Success, Try}

/**
 * This module is the main entry point into this library for command line
 * parsing and processing.
 *
 * ''ParameterManager'' acts as a facade for the other modules offered by this
 * library. It provides higher-level functions that delegate to other services
 * in order to combine multiple steps required to parse and process the
 * command line. These functions should be appropriate for the most use cases;
 * if there are special requirements, the underlying services can be used
 * directly, which enables a higher degree of customization.
 */
object ParameterManager {
  /**
   * Type definition for a function that does the initial parsing of the
   * command line arguments. The function expects the sequence of arguments as
   * input and categorizes the single elements into input arguments, options,
   * and their values. The resulting map with parameters is returned.
   */
  type ParsingFunc = Seq[String] => ParametersMap

  /**
   * A data class wrapping a ''CliExtractor'' and providing (lazy) access to
   * its metadata.
   *
   * When processing an application's command line it is typically necessary to
   * obtain metadata about the top-level ''CliExtractor''; based on this the
   * parsing of the elements on the command line can be done correctly. This
   * class manages this metadata and makes sure that it is obtained at most
   * once when it is actually needed.
   *
   * @param extractor the ''CliExtractor'' wrapped by this context
   * @tparam A the result type of the ''CliExtractor''
   */
  case class ExtractorContext[A](extractor: CliExtractor[A]) {
    /**
     * Stores a ''ParameterContext'' created based on the wrapped
     * ''CliExtractor''. From this context, all the metadata available about
     * the extractor can be queried.
     */
    lazy val parameterContext: ParameterContext = ParameterExtractor.gatherMetaData(extractor)

    /**
     * Stores a ''ModelContext'' created based on the wrapped ''CliExtractor''.
     */
    lazy val modelContext: ModelContext = parameterContext.modelContext
  }

  /**
   * Returns a list with standard ''ExtractedKeyClassifierFunc'' functions to
   * deal with elements on the command line. This list contains functions to
   * deal with all supported elements on the command line. The classifiers need
   * access to a ''ModelContext''. This is obtained from the
   * ''ExtractorContext'' provided.
   *
   * @param extractorCtx            the context wrapping the current extractor
   * @param supportCombinedSwitches flag whether combined switches are
   *                                supported
   * @return a list with standard ''ExtractedKeyClassifierFunc'' functions
   */
  def defaultExtractedKeyClassifiers(extractorCtx: ExtractorContext[_], supportCombinedSwitches: Boolean = false):
  List[ExtractedKeyClassifierFunc] = {
    lazy val resolverFunc = getAliasResolverFunc(extractorCtx)
    val switchClassifier = if (supportCombinedSwitches)
      ParameterParser.combinedSwitchKeyClassifierFunc(extractorCtx.modelContext)(resolverFunc)
    else ParameterParser.switchKeyClassifierFunc(extractorCtx.modelContext)(resolverFunc)
    List(ParameterParser.optionKeyClassifierFunc(extractorCtx.modelContext)(resolverFunc),
      switchClassifier,
      classifyUnknownOption)
  }

  /**
   * Returns a ''KeyExtractorFunc'' that is based on the given
   * ''OptionPrefixes'' object. The function detects keys of options or
   * switches starting with one of the prefix configured in the
   * ''OptionPrefixes'' object. If no prefixes are provided, the default ones
   * defined by [[ParameterParser.DefaultOptionPrefixes]] are used.
   *
   * @param prefixes the object with supported prefixes
   * @return a function to extract keys for options or switches
   */
  def defaultKeyExtractor(prefixes: OptionPrefixes = ParameterParser.DefaultOptionPrefixes): KeyExtractorFunc =
    prefixes.tryExtract

  /**
   * Returns a default ''CliClassifierFunc'' for the execution of the
   * ''CliExtractor'' provided. This function makes use of the default
   * ''ExtractedKeyClassifierFunc'' functions and uses the default prefixes for
   * options and switches. For the processing of switches, a mode can be
   * enabled in which multiple single-letter aliases can be combined in a
   * single parameter; so for instance, the parameter ''-cvfz'' is interpreted
   * as the four switches ''c'', ''v'', ''f'', and ''z''.
   *
   * @param extractorCtx            the context wrapping the current extractor
   * @param prefixes                the object defining prefixes for options
   * @param supportCombinedSwitches flag whether combined switches are
   *                                supported
   * @param keyExtractor            optional function to extract keys
   * @return the default ''CliClassifierFunc'' for this extractor
   */
  def classifierFunc(extractorCtx: ExtractorContext[_],
                     prefixes: OptionPrefixes = ParameterParser.DefaultOptionPrefixes,
                     supportCombinedSwitches: Boolean = false,
                     keyExtractor: KeyExtractorFunc = null): CliClassifierFunc =
    ParameterParser.classifierOf(defaultExtractedKeyClassifiers(extractorCtx,
      supportCombinedSwitches))(getOrDefault(keyExtractor, defaultKeyExtractor(prefixes)))

  /**
   * Returns a ''ParsingFunc'' that is configured with the parameters provided.
   * Using this function makes it easy to customize special properties of the
   * parsing process, such as the prefixes used to identify options. For
   * missing parameters default values are used.
   *
   * @param extractorCtx            the context wrapping the current extractor
   * @param prefixes                the object defining prefixes for options
   * @param supportCombinedSwitches flag whether combined switches are
   *                                supported
   * @return the configured parsing function
   */
  def parsingFunc(extractorCtx: ExtractorContext[_],
                  prefixes: OptionPrefixes = ParameterParser.DefaultOptionPrefixes,
                  supportCombinedSwitches: Boolean = false): ParsingFunc =
    parsingFuncForClassifier(extractorCtx)(classifierFunc(extractorCtx, prefixes, supportCombinedSwitches))

  /**
   * Returns a ''ParsingFunc'' that is configured with the
   * ''CliClassifierFunc'' provided.
   *
   * @param extractorCtx the context wrapping the current extractor
   * @param cf           the function to classify command line elements
   * @return the configured parsing function
   */
  def parsingFuncForClassifier(extractorCtx: ExtractorContext[_])(cf: CliClassifierFunc): ParsingFunc = {
    lazy val aliasResolverFunc: AliasResolverFunc = getAliasResolverFunc(extractorCtx)
    args =>
      ParameterParser.parseParameters(args)(cf)(aliasResolverFunc)
  }

  /**
   * Converts a ''CliExtractor'' that always returns a value into one that
   * returns a ''Try'' of this value.
   *
   * @param extractor the original extractor
   * @tparam A the result type of the extractor
   * @return the ''CliExtractor'' that returns a ''Try[A]''
   */
  def wrapTryExtractor[A](extractor: CliExtractor[A]): CliExtractor[Try[A]] =
    extractor.map(Try(_))

  /**
   * Convenience function for command line processing.
   *
   * This function creates an [[ExtractorContext]] for the given
   * ''CliExtractor'' and then delegates to ''processCommandLineCtx()''. Use
   * this function if you want to use the default parsing function. Otherwise,
   * it is more efficient to create an ''ExtractorContext'' manually, use it
   * for the configuration of the parsing function, and call
   * ''processCommandLineCtx()'' directly.
   *
   * @param args                      the sequence of command line arguments
   * @param extractor                 the ''CliExtractor'' to generate a result
   * @param parser                    an optional custom parsing function
   * @param checkUnconsumedParameters flag whether a check for unexpected
   *                                  parameters should be performed
   * @tparam A the result type of the ''CliExtractor''
   * @return a ''Try'' with the result of the extractor and the parameter
   *         context
   */
  def processCommandLine[A](args: Seq[String], extractor: CliExtractor[Try[A]], parser: ParsingFunc = null,
                            checkUnconsumedParameters: Boolean = true): Try[(A, ParameterContext)] =
    processCommandLineCtx(args, ExtractorContext(extractor), parser, checkUnconsumedParameters)

  /**
   * The main function for command line processing.
   *
   * This function uses a ''ParsingFunc'' to parse the given sequence of
   * command line arguments and runs the ''CliExtractor'' contained in the
   * ''ExtractorContext'' provided on the result. A ''Try'' with the result of
   * this extractor and the parameter context is returned. If the extraction
   * process fails, result is a ''Failure'' that contains a
   * [[com.github.scli.ParameterExtractor#ParameterExtractionException]].
   * From this exception, all information is available to generate a
   * meaningful error message and usage information. Specifically, the failure
   * messages have already been added to the model context in the parameter
   * context available via the exception.
   *
   * @param args                      the sequence of command line arguments
   * @param extractorCtx              the context wrapping the current
   *                                  extractor
   * @param parser                    an optional custom parsing function
   * @param checkUnconsumedParameters flag whether a check for unexpected
   *                                  parameters should be performed
   * @tparam A the result type of the ''CliExtractor''
   * @return a ''Try'' with the result of the extractor and the parameter
   *         context
   */
  def processCommandLineCtx[A](args: Seq[String], extractorCtx: ExtractorContext[Try[A]], parser: ParsingFunc = null,
                               checkUnconsumedParameters: Boolean = true): Try[(A, ParameterContext)] = {
    val theParsingFunc = getOrDefault(parser, parsingFunc(extractorCtx))
    extract(theParsingFunc(args), extractorCtx.extractor, checkUnconsumedParameters)
  }

  /**
   * Processes options referring to parameter files and returns the resulting,
   * augmented command line. This function can be called before processing of
   * the command if the CLI should support reading parameters from files. It
   * searches for options referencing such files based on the
   * ''FileOptionFunc'' provided. The files are read, and their content is
   * placed on the command line. The resulting command line is returned. If
   * reading a parameter file causes an error, the resulting ''Try'' fails with
   * a ''ParameterExtractionException'' pointing to the option responsible for
   * the failure.
   *
   * @param args           the original sequence with command line arguments
   * @param extractorCtx   the context wrapping the current extractor
   * @param cf             the classifier function
   * @param fileOptionFunc a function to detect file options
   * @return a ''Try'' with the processed command line
   */
  def processParameterFiles(args: Seq[String], extractorCtx: ExtractorContext[_])(cf: CliClassifierFunc)
                           (fileOptionFunc: FileOptionFunc): Try[Seq[String]] =
    handleParameterFileExceptions(ParameterParser.processParameterFiles(args)(cf)(fileOptionFunc),
      extractorCtx.extractor)

  /**
   * Searches for the options specified on the command line and processes the
   * files with parameters they refer to. This function is analogous to
   * ''processParameterFiles()'', but it constructs a ''FileOptionFunc'' that
   * recognizes options with the given keys.
   *
   * @param args         the original sequence with command line arguments
   * @param extractorCtx the context wrapping the current extractor
   * @param options      the options referencing parameter files
   * @param cf           the classifier function
   * @return
   */
  def processParameterFilesWithOptions(args: Seq[String], extractorCtx: ExtractorContext[_], options: ParameterKey*)
                                      (cf: CliClassifierFunc): Try[Seq[String]] = {
    val fileOptionFunc = ParameterParser.fileOptionFuncForOptions(options)
    processParameterFiles(args, extractorCtx)(cf)(fileOptionFunc)
  }

  /**
   * Handles an exception caused by a failure to process a parameter file.
   * Processing of parameter files can fail, e.g. if a parameter file cannot be
   * found. In this case, [[ParameterParser]] produces a specific exception.
   * This function checks whether the ''Try'' passed in has failed with this
   * exception. If so, the exception is converted to a
   * [[ParameterExtractionException]] that can be used to display an error text
   * in the usual way. ''Note'': When calling one of the
   * ''processParameterFiles()'' functions, this function is invoked
   * automatically; it should be used only if the [[ParameterParser]] was
   * called directly.
   *
   * @param processedArgs the ''Try'' with the processed command line
   * @param extractor     the current extractor
   * @return the modified ''Try'' with the exception handled
   */
  def handleParameterFileExceptions(processedArgs: Try[Seq[String]], extractor: CliExtractor[_]): Try[Seq[String]] =
    processedArgs recoverWith {
      case e: ParameterFileException =>
        val context = gatherMetaData(extractor)
        val failure = ExtractionFailure(e.fileOption, e.getMessage, context)
        Failure(updateModelContextWithFailures(List(failure), context))
    }

  /**
   * Handles the extraction of data objects from the command line and
   * optionally checks for unconsumed parameters. In case of failures, it is
   * ensured that all messages are contained in the resulting exception.
   *
   * @param params                    the map with parameters
   * @param extractor                 the extractor
   * @param checkUnconsumedParameters flag whether a check for unexpected
   *                                  parameters should be performed
   * @tparam A the result type of the ''CliExtractor''
   * @return a ''Try'' with the result of the extractor and the parameter
   *         context
   */
  private def extract[A](params: ParametersMap, extractor: CliExtractor[Try[A]], checkUnconsumedParameters: Boolean):
  Try[(A, ParameterContext)] = {
    val (res, context) = runExtractor(extractor, params)(DefaultConsoleReader)
    val triedContext = checkParametersConsumedConditionally(context, checkUnconsumedParameters)
    createRepresentation(res, triedContext) {
      (_, _)
    } recoverWith {
      case e: ParameterExtractionException =>
        Failure(updateModelContextWithFailures(e.failures, context))
    }
  }

  /**
   * Performs a check for unconsumed parameters if this check is enabled.
   * Otherwise, a success result is returned.
   *
   * @param context the parameter context to check
   * @param enabled flag whether the check is enabled
   * @return a ''Try'' with the checked parameter context
   */
  private def checkParametersConsumedConditionally(context: ParameterContext, enabled: Boolean):
  Try[ParameterContext] =
    if (enabled) checkParametersConsumed(context)
    else Success(context)

  /**
   * Generates an updated model context that contains all the failures of the
   * passed in list. Then the failures are updated to reference the
   * parameter context with the updated model context, and a new exception with
   * these failures is created.
   *
   * @param failures the original list of failures
   * @param context  the most recent parameter context
   * @return an updated exception referencing the new model context
   */
  private def updateModelContextWithFailures(failures: List[ExtractionFailure], context: ParameterContext):
  ParameterExtractionException = {
    val helpContext = addFailuresToModelContext(context.modelContext, failures)
    val newContext = context.copy(modelContext = helpContext)
    val newFailures = failures.map(_.copy(context = newContext))
    ParameterExtractionException(newFailures)
  }

  /**
   * Function used as ''ExtractedKeyClassifierFunc'' for unknown options or
   * switches. This function returns a defined result, although the parameter
   * key is unknown. That this parameter is unsupported will be discovered
   * later in the extraction phase.
   *
   * @param key   the current key
   * @param args  the sequence with arguments
   * @param index the current parameter index
   * @return an ''Option'' with the element identified
   */
  private def classifyUnknownOption(key: ParameterKey, args: Seq[String], index: Int): Option[CliElement] = {
    val elem = if (index >= args.size - 1) SwitchesElement(List((key, "true")))
    else OptionElement(key, Some(args(index + 1)))
    Some(elem)
  }

  /**
   * Returns an ''AliasResolverFunc'' that is based on the alias mapping
   * obtained from the ''ExtractorContext'' provided.
   *
   * @param extractorCtx the ''ExtractorContext''
   * @return the function to resolve alias keys
   */
  private def getAliasResolverFunc(extractorCtx: ExtractorContext[_]): AliasResolverFunc =
    extractorCtx.modelContext.aliasMapping.keyForAlias.get

  /**
   * Helper function to replace a '''null''' value by a default value.
   *
   * @param value   the value, which can be '''null'''
   * @param default the lazy default value
   * @tparam A the type of the value
   * @return the value if defined; the default value otherwise
   */
  private def getOrDefault[A](value: A, default: => A): A =
    Option(value) getOrElse default
}
