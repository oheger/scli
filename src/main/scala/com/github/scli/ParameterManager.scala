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
   * A data class wrapping a ''CliExtractor'' and defining additional
   * properties for an extract operation.
   *
   * Objects of this class can be passed to functions to parse and process the
   * command line. They contain the ''CliExtractor'' to be used and allow for
   * an advanced customization of the operation. For all options default values
   * are set; so on creation only relevant properties need to be specified.
   *
   * Another purpose served by this class is providing access to metadata about
   * the parameters supported by the application. When processing an
   * application's command line it is typically necessary to obtain metadata
   * about the top-level ''CliExtractor''; based on this the parsing of the
   * elements on the command line can be done correctly. This class manages
   * this metadata and makes sure that it is obtained at most once when it is
   * actually needed.
   *
   * @param extractor               the wrapped ''CliExtractor''
   * @param prefixes                the object defining prefixes for options
   * @param supportCombinedSwitches flag whether combined switches are
   *                                supported
   * @param keyExtractor            optional function to extract keys
   * @param fileOptions             parameter keys referencing parameter files
   * @tparam A the result type of the ''CliExtractor''
   */
  case class ExtractionSpec[A](extractor: CliExtractor[A],
                               prefixes: OptionPrefixes = ParameterParser.DefaultOptionPrefixes,
                               supportCombinedSwitches: Boolean = false,
                               keyExtractor: KeyExtractorFunc = null,
                               fileOptions: Seq[ParameterKey] = Nil) {
    /**
     * Stores a ''ParameterContext'' created based on the wrapped
     * ''CliExtractor''. From this context, all the metadata available about
     * the extractor can be queried.
     */
    lazy val parameterContext: ParameterContext = constructParameterContext()

    /**
     * Stores a ''ModelContext'' created based on the wrapped ''CliExtractor''.
     */
    lazy val modelContext: ModelContext = parameterContext.modelContext

    /**
     * Creates a ''ParameterContext'' for the ''CliExtractor'' contained in
     * this object.
     *
     * @return the ''ParameterContext''
     */
    private def constructParameterContext(): ParameterContext =
      addFileOptionsToModelContext(ParameterExtractor.gatherMetaData(extractor))

    /**
     * Adds the options referencing parameter files to the model context. This
     * is necessary, so that they can be classified correctly under all
     * circumstances.
     *
     * @param paramCtx the original parameter context
     * @return the extended parameter context
     */
    private def addFileOptionsToModelContext(paramCtx: ParameterContext): ParameterContext = {
      val modelContext = fileOptions.foldLeft(paramCtx.modelContext) { (ctx, key) =>
        ctx.addOption(key, None)
      }
      paramCtx.copy(modelContext = modelContext)
    }
  }

  /**
   * Returns a list with standard ''ExtractedKeyClassifierFunc'' functions to
   * deal with elements on the command line. This list contains functions to
   * deal with all supported elements on the command line. The classifiers need
   * access to a ''ModelContext''. This is obtained from the
   * ''ExtractionSpec'' provided.
   *
   * @param spec the spec for the extraction operation
   * @return a list with standard ''ExtractedKeyClassifierFunc'' functions
   */
  def defaultExtractedKeyClassifiers(spec: ExtractionSpec[_]):
  List[ExtractedKeyClassifierFunc] = {
    lazy val resolverFunc = getAliasResolverFunc(spec)
    val switchClassifier = if (spec.supportCombinedSwitches)
      ParameterParser.combinedSwitchKeyClassifierFunc(spec.modelContext)(resolverFunc)
    else ParameterParser.switchKeyClassifierFunc(spec.modelContext)(resolverFunc)
    List(ParameterParser.optionKeyClassifierFunc(spec.modelContext)(resolverFunc),
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
   * @param spec the spec for the extraction operation
   * @return the default ''CliClassifierFunc'' for this extractor
   */
  def classifierFunc(spec: ExtractionSpec[_]): CliClassifierFunc =
    ParameterParser.classifierOf(defaultExtractedKeyClassifiers(spec))(getOrDefault(spec.keyExtractor,
      defaultKeyExtractor(spec.prefixes)))

  /**
   * Returns a ''ParsingFunc'' that is configured with the parameters provided.
   * Using this function makes it easy to customize special properties of the
   * parsing process, such as the prefixes used to identify options. For
   * missing parameters default values are used.
   *
   * @param spec the spec for the extraction operation
   * @return the configured parsing function
   */
  def parsingFunc(spec: ExtractionSpec[_]): ParsingFunc =
    parsingFuncForClassifier(spec)(classifierFunc(spec))

  /**
   * Returns a ''ParsingFunc'' that is configured with the
   * ''CliClassifierFunc'' provided.
   *
   * @param spec the spec for the extraction operation
   * @param cf   the function to classify command line elements
   * @return the configured parsing function
   */
  def parsingFuncForClassifier(spec: ExtractionSpec[_])(cf: CliClassifierFunc): ParsingFunc = {
    lazy val aliasResolverFunc: AliasResolverFunc = getAliasResolverFunc(spec)
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
   * This function creates an [[ExtractionSpec]] for the given
   * ''CliExtractor'' (using defaults) and then delegates to
   * ''processCommandLineSpec()''. Use this function if you want to use the
   * default parsing function. Otherwise, it is more efficient to create an
   * [[ExtractionSpec]] manually, use it for the configuration of the parsing
   * function, and call ''processCommandLineSpec()'' directly.
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
    processCommandLineSpec(args, ExtractionSpec(extractor), parser, checkUnconsumedParameters)

  /**
   * The main function for command line processing.
   *
   * This function uses a ''ParsingFunc'' to parse the given sequence of
   * command line arguments and runs the ''CliExtractor'' contained in the
   * ''ExtractionSpec'' provided on the result. A ''Try'' with the result of
   * this extractor and the parameter context is returned. If the extraction
   * process fails, result is a ''Failure'' that contains a
   * [[com.github.scli.ParameterExtractor#ParameterExtractionException]].
   * From this exception, all information is available to generate a
   * meaningful error message and usage information. Specifically, the failure
   * messages have already been added to the model context in the parameter
   * context available via the exception.
   *
   * @param args                      the sequence of command line arguments
   * @param spec                      the spec for the extraction operation
   * @param parser                    an optional custom parsing function
   * @param checkUnconsumedParameters flag whether a check for unexpected
   *                                  parameters should be performed
   * @tparam A the result type of the ''CliExtractor''
   * @return a ''Try'' with the result of the extractor and the parameter
   *         context
   */
  def processCommandLineSpec[A](args: Seq[String], spec: ExtractionSpec[Try[A]], parser: ParsingFunc = null,
                                checkUnconsumedParameters: Boolean = true): Try[(A, ParameterContext)] = {
    val theParsingFunc = getOrDefault(parser, parsingFunc(spec))
    extract(theParsingFunc(args), spec.extractor, checkUnconsumedParameters)
  }

  /**
   * Processes options referring to parameter files and returns the resulting,
   * augmented command line. This function can be called before processing of
   * the command if the CLI should support reading parameters from files. It
   * searches for options referencing such files as defined by the
   * ''ExtractionSpec'' provided. The files are read, and their content is
   * placed on the command line. The resulting command line is returned. If
   * reading a parameter file causes an error, the resulting ''Try'' fails with
   * a ''ParameterExtractionException'' pointing to the option responsible for
   * the failure.
   *
   * @param args the original sequence with command line arguments
   * @param spec the spec for the extraction operation
   * @param cf   the classifier function
   * @return
   */
  def processParameterFiles(args: Seq[String], spec: ExtractionSpec[_])
                           (cf: CliClassifierFunc): Try[Seq[String]] = {
    val fileOptionFunc = ParameterParser.fileOptionFuncForOptions(spec.fileOptions)
    handleParameterFileExceptions(ParameterParser.processParameterFiles(args)(cf)(fileOptionFunc), spec.extractor)
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
   * obtained from the ''ExtractionSpec'' provided.
   *
   * @param spec the spec for the extraction operation
   * @return the function to resolve alias keys
   */
  private def getAliasResolverFunc(spec: ExtractionSpec[_]): AliasResolverFunc =
    spec.modelContext.aliasMapping.keyForAlias.get

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
