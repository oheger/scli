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

import java.text.MessageFormat

import com.github.scli.ParameterExtractor._
import com.github.scli.ParameterModel.{FailureContext, ModelContext, ParameterKey}
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
   * An extractor for the help flag on the command line that is used if the
   * application does not specify such an extractor. This extractor always
   * reports a disabled help flag.
   */
  private val DefaultHelpExtractor: CliExtractor[Try[Boolean]] = constantExtractor(Success(false))

  /**
   * A map with default failure messages for the exception generator function.
   */
  private val ExceptionMessages =
    Map(FailureCodes.TooManyInputParameters -> "Too many input arguments; expected at most {0}",
      FailureCodes.UnknownGroup -> "Invalid value \"{0}\". Expected one of {1}",
      FailureCodes.MultipleValues -> "Single value expected, but got {0}",
      FailureCodes.MandatoryParameterMissing -> "Mandatory parameter has no value",
      FailureCodes.MultiplicityTooLow -> "Too few values; parameter must have at least {0} values",
      FailureCodes.MultiplicityTooHigh -> "Too many values; parameter must have at most {0} values",
      FailureCodes.UnsupportedParameter -> "Unsupported parameter")

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
   * @param optHelpExtractor        optional ''CliExtractor'' to determine
   *                                whether the user has requested help; this
   *                                could be for instance a ''--help'' switch
   * @param exceptionGenerator      the exception generator function
   * @param optExceptionMapper      optional exception mapper function
   * @tparam A the result type of the ''CliExtractor''
   */
  case class ExtractionSpec[A](extractor: CliExtractor[Try[A]],
                               prefixes: OptionPrefixes = ParameterParser.DefaultOptionPrefixes,
                               supportCombinedSwitches: Boolean = false,
                               keyExtractor: KeyExtractorFunc = null,
                               fileOptions: Seq[ParameterKey] = Nil,
                               optHelpExtractor: Option[CliExtractor[Try[Boolean]]] = None,
                               exceptionGenerator: ExceptionGenerator = ParameterManager.defaultExceptionGenerator,
                               optExceptionMapper: Option[ExceptionMapper] = None) {
    /**
     * Stores an internal extractor, which gets executed for this
     * ''ExtractionSpec''. This extractor not only extracts the actual result
     * for the application but also a help flag.
     */
    private[scli] val internalExtractor = constructInternalExtractor()

    /**
     * Stores a ''ExtractionContext'' created based on the wrapped
     * ''CliExtractor''. From this context, all the metadata available about
     * the extractor can be queried.
     */
    lazy val extractionContext: ExtractionContext = constructExtractionContext()

    /**
     * Stores a ''ModelContext'' created based on the wrapped ''CliExtractor''.
     */
    lazy val modelContext: ModelContext = extractionContext.modelContext

    /**
     * Constructs the internal extractor used by this specification. The
     * internal extractor extends the main extractor by checking for a help
     * flag.
     *
     * @return the internal extractor
     */
    private def constructInternalExtractor(): CliExtractor[Try[(A, Boolean)]] =
      createExtractorWithHelpFlag(extractor,
        createInternalHelpExtractor(optHelpExtractor getOrElse DefaultHelpExtractor))

    /**
     * Creates an ''ExtractionContext'' for the ''CliExtractor'' contained in
     * this object.
     *
     * @return the ''ExtractionContext''
     */
    private def constructExtractionContext(): ExtractionContext =
      addFileOptionsToModelContext(ParameterExtractor.gatherMetaData(internalExtractor))

    /**
     * Adds the options referencing parameter files to the model context. This
     * is necessary, so that they can be classified correctly under all
     * circumstances.
     *
     * @param extrCtx the original extraction context
     * @return the extended extraction context
     */
    private def addFileOptionsToModelContext(extrCtx: ExtractionContext): ExtractionContext = {
      val modelContext = fileOptions.foldLeft(extrCtx.modelContext) { (ctx, key) =>
        ctx.addOption(key, None)
      }
      extrCtx.copy(modelContext = modelContext)
    }
  }

  /**
   * A data class storing context information about an operation to process the
   * command line.
   *
   * This class extends the [[ExtractionContext]] used by extraction operations
   * by additional metadata. It especially stores information whether a flag
   * was found on the command line requesting help and failure information.
   * This information is needed to correctly interpret the result of processing
   * a command line and do proper error handling.
   *
   * @param parameterContext  the ''ExtractionContext'' generated during the
   *                          extraction phase
   * @param helpRequested     flag whether help was requested by the user
   * @param optFailureContext optional context with failure information, which
   *                          is present if failures have been detected
   */
  case class ProcessingContext(parameterContext: ExtractionContext,
                               helpRequested: Boolean,
                               optFailureContext: Option[FailureContext])

  /**
   * Type definition for the result of an operation to process the command
   * line. If the operation was successful, the result consists of the value
   * produced by the [[CliExtractor]] and the initialized
   * ''ProcessingContext''; as the operation can fail, this is wrapped in a
   * ''Try''. If the extraction process fails, result is a ''Failure'' that
   * contains a
   * [[com.github.scli.ParameterExtractor#ParameterExtractionException]].
   * From this exception, all information is available to generate a
   * meaningful error message and usage information. Specifically, the failure
   * messages have already been added to the model context in the parameter
   * context available via the exception.
   */
  type ProcessingResult[A] = Try[(A, ProcessingContext)]

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
   * Returns the default exception generator function. This function handles
   * all the standard error codes used by the [[ParameterExtractor]] module.
   * It generates exceptions of type ''IllegalArgumentException'' with standard
   * error messages.
   *
   * @return the default ''ExceptionGenerator'' function
   */
  def defaultExceptionGenerator: ExceptionGenerator =
    exceptionGenerator(ExceptionMessages)

  /**
   * Returns an exception generator function that produces exceptions of type
   * ''IllegalArgumentException'' with messages defined by the given map. The
   * map contains messages for the failure codes supported by the
   * [[ParameterExtractor]] module. The final messages are generated from the
   * parameters provided using Java's ''MessageFormat'' class; so they can
   * contain placeholders like ''{0}'' or ''{1}'' that are replaced by current
   * parameter values.
   *
   * The passed in map with parameters need not contain texts for all possible
   * failure codes; for missing codes, the default messages are used.
   *
   * @param messages the map with failure messages
   * @return an ''ExceptionGenerator'' function that uses these messages
   */
  def exceptionGenerator(messages: Map[FailureCodes.Value, String]): ExceptionGenerator = {
    val allMessages = ExceptionMessages ++ messages
    (_, code, params) =>
      new IllegalArgumentException(MessageFormat.format(allMessages(code), params: _*))
  }

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
                            checkUnconsumedParameters: Boolean = true): ProcessingResult[A] =
    processCommandLineSpec(args, ExtractionSpec(extractor), parser, checkUnconsumedParameters)

  /**
   * The main function for command line processing.
   *
   * This function uses a ''ParsingFunc'' to parse the given sequence of
   * command line arguments and runs the ''CliExtractor'' contained in the
   * ''ExtractionSpec'' provided on the result. The final outcome is returned
   * as a ''ProcessingResult''.
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
  def processCommandLineSpec[A](args: Seq[String], spec: ExtractionSpec[A], parser: ParsingFunc = null,
                                checkUnconsumedParameters: Boolean = true): ProcessingResult[A] = {
    val theParsingFunc = getOrDefault(parser, parsingFunc(spec))
    extract(theParsingFunc(args), spec, checkUnconsumedParameters)
      .map(t => (t._1._1, ProcessingContext(t._2, helpRequested = t._1._2, optFailureContext = None)))
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
        val failure = ExtractionFailure(e.fileOption, e, None, context)
        Failure(ParameterExtractionException(List(failure)))
    }

  /**
   * A function that simplifies the evaluation of a ''ProcessingResult''. This
   * function supports an application in the decision whether it can continue
   * with the processing of the processing result or should display a help
   * message. If command line processing encountered failures or if the user
   * has explicitly requested help (by setting a help switch on the command
   * line), result is a ''Left'' with a ''ProcessingContext'' that can be used
   * to display help information. Otherwise, all parameters could be parsed
   * successfully, and the result is available as a ''Right'' to be consumed by
   * the application.
   *
   * Note: In case of a failed result, this function assumes that the exception
   * is of type [[ParameterExtractionException]]; if a different exception is
   * found (which should not happen when the result was constructed by this
   * module), an ''IllegalArgumentException'' exception is thrown.
   *
   * @param result the ''ProcessingResult'' to be evaluated
   * @tparam A the type of the actual result produced by the extractor
   * @return an ''Either'' simplifying the interpretation of the result
   */
  def evaluate[A](result: ProcessingResult[A]): Either[ProcessingContext, A] =
    result match {
      case Success((_, context)) if context.helpRequested =>
        Left(context)
      case Success((result, _)) =>
        Right(result)
      case Failure(e: ParameterExtractionException) =>
        Left(ProcessingContext(e.extractionContext, helpRequested = false,
          optFailureContext = Some(new FailureContext(e.extractionContext.modelContext, e.failures))))
      case Failure(e) =>
        throw new IllegalArgumentException("Unexpected failure in ProcessingResult", e)
    }

  /**
   * Handles the extraction of data objects from the command line and
   * optionally checks for unconsumed parameters. In case of failures, it is
   * ensured that all messages are contained in the resulting exception.
   *
   * @param params                    the map with parameters
   * @param spec                      the extractor
   * @param checkUnconsumedParameters flag whether a check for unexpected
   *                                  parameters should be performed
   * @tparam A the result type of the ''CliExtractor''
   * @return a ''Try'' with the result of the extractor and the parameter
   *         context
   */
  private def extract[A](params: ParametersMap, spec: ExtractionSpec[A], checkUnconsumedParameters: Boolean):
  Try[((A, Boolean), ExtractionContext)] = {
    val extrCtx = ExtractionContext(params, ParameterModel.EmptyModelContext, DefaultConsoleReader,
      spec.exceptionGenerator, spec.optExceptionMapper)
    val (res, context) = runExtractor(spec.internalExtractor, extrCtx)
    val triedContext = checkParametersConsumedConditionally(context, checkUnconsumedParameters)
    createRepresentation(res, triedContext)((_, _)) recoverWith {
      case e: ParameterExtractionException =>
        Failure(updateContextInFailures(e.failures, context))
    }
  }

  /**
   * Performs a check for unconsumed parameters if this check is enabled.
   * Otherwise, a success result is returned.
   *
   * @param context the extraction context to check
   * @param enabled flag whether the check is enabled
   * @return a ''Try'' with the checked extraction context
   */
  private def checkParametersConsumedConditionally(context: ExtractionContext, enabled: Boolean):
  Try[ExtractionContext] =
    if (enabled) checkParametersConsumed(context)
    else Success(context)

  /**
   * Generates an updated exception with a list of failures that contain the
   * newest  model context that contains all the failures of the
   * passed in list. Then the failures are updated to reference the
   * extraction context with the updated model context, and a new exception with
   * these failures is created.
   *
   * @param failures the original list of failures
   * @param context  the most recent extraction context
   * @return an updated exception referencing the new model context
   */
  private def updateContextInFailures(failures: List[ExtractionFailure], context: ExtractionContext):
  ParameterExtractionException = {
    val newFailures = failures.map(_.copy(context = context))
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

  /**
   * Generates a ''CliExtractor'' that evaluates a help parameter in addition
   * to the regular parameters supported by an application.
   *
   * @param mainExtractor the main data extractor
   * @param helpExtractor the extractor of the help flag
   * @tparam A the type of the main extractor
   * @return an extractor for the data and the help flag
   */
  private def createExtractorWithHelpFlag[A](mainExtractor: CliExtractor[Try[A]],
                                             helpExtractor: CliExtractor[Try[Boolean]]):
  CliExtractor[Try[(A, Boolean)]] =
    for {
      help <- helpExtractor
      data <- mainExtractor
    } yield createRepresentation(data, help) {
      (_, _)
    }

  /**
   * Creates the extractor that is used internally to check whether the user
   * has requested help. This function creates an extractor derived from the
   * one passed in, which replaces the [[ConsoleReader]] in the parameter
   * context if a help request is detected. This prevents that the user is
   * prompted on the console to enter missing passwords before the help screen
   * is displayed.
   *
   * @param ext the original help extractor
   * @return the internal, extended help extractor
   */
  private def createInternalHelpExtractor(ext: CliExtractor[Try[Boolean]]): CliExtractor[Try[Boolean]] =
    ext mapWithContext { (result, context) =>
      val updContext = result match {
        case Success(true) =>
          context.copy(reader = DummyConsoleReader)
        case Failure(_) =>
          context.copy(reader = DummyConsoleReader)
        case _ => context
      }
      (result, updContext)
    }
}
