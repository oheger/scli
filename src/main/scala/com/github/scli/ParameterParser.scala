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

import com.github.scli.ParameterModel.{ModelContext, ParameterAttributeKey, ParameterKey}

import scala.annotation.tailrec
import scala.io.Source
import scala.util.{Failure, Success, Try}

/**
 * A module for parsing command line arguments into options and input values.
 *
 * This module provides a function that expects a sequence of command line
 * arguments as input and tries to generate a map with parameters from it.
 * This function groups the single strings on the command line into option
 * names, option values, and other input parameters. The resulting
 * representation of parameters can then serve as input for the further
 * processing of the command line, e.g. to extract configuration objects out
 * of it.
 */
object ParameterParser {
  /**
   * Reserved key of a parameter that collects the input strings that are no
   * values of options or switches.
   */
  final val InputParameter = ParameterKey("input", shortAlias = false, hasPrefix = false)

  /**
   * An OptionPrefixes object with the default prefix for options. This is
   * used if for a parse operation no explicit functions to recognize options
   * and extract their keys are specified.
   */
  final val DefaultOptionPrefixes = OptionPrefixes(ParameterKey("--", shortAlias = false),
    ParameterKey("-", shortAlias = true))

  /**
   * Type definition for the map with resolved parameter values. The array
   * with command line options is transformed in such a map which allows
   * direct access to the value(s) assigned to parameters. The map stores
   * ''CliElement'' objects, so that the original information from the command
   * line is still available.
   */
  type ParametersMap = Map[ParameterKey, Iterable[CliElement]]

  /**
   * A trait describing an item encountered on the command line.
   *
   * During parameter parsing the single items on the command line need to be
   * classified into options, switches, or input parameters. For this purpose,
   * a classification function is used which returns sub classes of this trait.
   * The parser needs to handle the sub classes differently.
   *
   * This trait also plays a role for error handling: It stores the original
   * key and the raw value of the represented parameter. This information is
   * useful when generating meaningful error messages.
   *
   * In addition, the position of the corresponding element on the command line
   * can be queried. This is needed by some use cases that override the values
   * of options with values appearing later on the command line.
   */
  sealed trait CliElement {
    /**
     * Returns the key used for this parameter value on the command line. While
     * values are stored under the main parameter key, this method allows
     * determining the key the user has specified (which may be an alias).
     *
     * @return the key used for this ''CliElement''
     */
    def key: ParameterKey

    /**
     * Returns the raw value of this ''CliElement'' as specified by the user.
     * The value may be transformed during the extraction phase. If
     * transformation fails, it might be useful to have the original value to
     * construct an error message.
     *
     * @return the value of this ''CliElement''
     */
    def value: String

    /**
     * Returns the (0-based) index where this element was placed on the command
     * line. This makes it possible to sort command line items by their
     * position they have been placed on the command line.
     *
     * @return the index of this ''CliElement'' on the command line
     */
    def index: Int
  }

  /**
   * A concrete ''CliElement'' class representing an option.
   *
   * The option is identified by its key. If possible, its value is extracted;
   * but this may noe be possible, for instance if the option key was the last
   * parameter on the command line.
   *
   * @param key      the key of the option
   * @param optValue an ''Option'' with the value
   * @param index    the index of this element on the command line
   */
  case class OptionElement(override val key: ParameterKey,
                           optValue: Option[String],
                           override val index: Int = 0) extends CliElement {
    override def value: String = optValue getOrElse ""
  }

  /**
   * A concrete ''CliElement'' class representing a set of switches.
   *
   * A single parameter on the command line can contain multiple switches, e.g.
   * if multiple short keys are composed, as in ''tar xvzf''. Therefore, this
   * class holds a list with switches that have been extracted. For each switch
   * its key and its value (which is derived from the default value defined in
   * the parameter model) is stored.
   *
   * Note: The properties for key and value are dummies; during parameter
   * parsing, for each switch in the result, a new element is created.
   *
   * @param switches a list with switches and their values that have been
   *                 extracted
   * @param index    the index of this element on the command line
   */
  case class SwitchesElement(switches: List[(ParameterKey, String)],
                             override val index: Int = 0) extends CliElement {
    override def key: ParameterKey = switches.head._1

    override def value: String = switches.head._2
  }

  /**
   * A concrete ''CliElement'' class representing an input parameter.
   *
   * @param value the value to be added to the input parameters
   * @param index the index of this element on the command line
   */
  case class InputParameterElement(override val value: String,
                                   override val index: Int = 0) extends CliElement {
    override val key: ParameterKey = InputParameter
  }

  /**
   * Definition of a function to classify parameters on the command line.
   *
   * The function is passed the sequence with parameters and the index of the
   * current one to be inspected. It returns the result of the classification
   * as a [[CliElement]] object.
   */
  type CliClassifierFunc = (Seq[String], Int) => CliElement

  /**
   * Definition of a function to classify specific parameters whose key has
   * already been extracted.
   *
   * This function type is similar to [[CliClassifierFunc]], but there are a
   * couple of differences: First, it is invoked with a parameter key in
   * addition to the sequence of parameters and the current index; so it will
   * be called only for options or switches. Second, this function can fail to
   * classify a parameter. The idea here is that multiple functions of this
   * type can be combined, each of which is specialized for a specific
   * parameter type. If none of those are able to detect the parameter type,
   * an input parameter can be assumed as fallback.
   */
  type ExtractedKeyClassifierFunc = (ParameterKey, Seq[String], Int) => Option[CliElement]

  /**
   * Definition of a function that can extract the key of an option or switch
   * parameter from a command line element.
   *
   * The function is passed the string encountered on the command line. A
   * concrete implementation probably checks whether this string starts with a
   * prefix indicating an option or switch. If so, the key is returned (with
   * the prefix removed); otherwise, result is an empty ''Option''.
   */
  type KeyExtractorFunc = String => Option[ParameterKey]

  /**
   * Definition of a function that can resolve aliases.
   *
   * The idea is that the function gets passed in a parameter key. If this key
   * is known to be an alias, the function should return a defined ''Option''
   * with the key the alias is for; this key is then used as replacement for
   * the alias. If the key cannot be resolved as an alias, the function should
   * return ''None''; the key is then used as is.
   */
  type AliasResolverFunc = ParameterKey => Option[ParameterKey]

  /**
   * Definition of a function that detects options referencing a parameters
   * file.
   *
   * The library supports externalizing complex parameter lists in files.
   * These files can be specified on the command line using specific options.
   * This function type is used by the ''processFileOptions()'' function to
   * determine the parameter files to be loaded. The function is passed a
   * [[CliElement]]; if this element references a parameters file, it returns a
   * defined ''Option'' with the key of the file option (to be used in error
   * messages) and the file name.
   */
  type FileOptionFunc = CliElement => Option[(ParameterKey, String)]

  object OptionPrefixes {
    /**
     * Returns a new instance of ''OptionPrefixes'' that accepts the prefixes
     * passed in.
     *
     * @param prefixes the supported option prefixes
     * @return the new ''OptionPrefixes'' instance
     */
    def apply(prefixes: ParameterKey*): OptionPrefixes = {
      new OptionPrefixes(prefixes.toList)
    }
  }

  /**
   * A data class that stores a list with supported prefixes for options.
   *
   * When parsing the command line each item is checked whether it starts with
   * one of the prefixes defined by this class. If so, the item is considered
   * an option or a switch.
   *
   * Command line applications often distinguish between full parameter names
   * and short alias names. The alias has the same meaning as the regular
   * parameter name, but it is shorter to type on the command line. Typically,
   * a different prefix is used distinguish between the long and short
   * parameter names, such as ''--target-directory'' for the full name and
   * ''-d'' for the short alias. When constructing an instance the prefixes to
   * be managed are specified as [[ParameterKey]] objects; hence, it can be
   * stated whether a prefix is used for long or short parameter names.
   *
   * @param prefixes the supported option prefixes
   */
  case class OptionPrefixes private(prefixes: List[ParameterKey]) {
    /**
     * Stores the prefixes sorted by their lengths. This makes sure that they
     * are processed in correct order, if one prefix starts with another one.
     */
    private val sortedPrefixes = prefixes.sortWith(_.key.length > _.key.length)

    /**
     * Tries to extract the key of an option from the given parameter. The
     * function checks whether the parameter has one of the prefixes configured
     * for this object. If so, the key without the prefix is returned.
     * Otherwise, result is an empty ''Option''.
     *
     * @param parameter the parameter to process
     * @return an ''Option'' with the key extracted
     */
    def tryExtract(parameter: String): Option[ParameterKey] =
      findPrefix(parameter).map { prefix =>
        ParameterKey(parameter drop prefix.key.length, prefix.shortAlias)
      }

    /**
     * Returns an ''Option'' with the prefix that matches the passed in option
     * key.
     *
     * @param key the option key
     * @return an ''Option'' with the prefix the key starts with
     */
    private def findPrefix(key: String): Option[ParameterKey] =
      sortedPrefixes find (pk => key.startsWith(pk.key))
  }

  /**
   * A specialized exception class to report problems during the processing of
   * parameter files.
   *
   * During parameter file processing, I/O exceptions can occur. Such
   * exceptions are converted to exceptions of this type. They transport some
   * more information which is useful when handling errors, e.g. printing help
   * or error information.
   *
   * @param msg        an error message
   * @param cause      the causing exception, typically an ''IOException''
   * @param fileOption the key of the option to read parameter files
   */
  class ParameterFileException(msg: String,
                               cause: Throwable,
                               val fileOption: ParameterKey) extends Exception(msg, cause)

  /**
   * Type definition for an internal map type used during processing of
   * command line arguments.
   */
  private type InternalParamMap = Map[ParameterKey, List[CliElement]]

  /**
   * Generates a ''CliClassifierFunc'' from the given sequence extracted key
   * classifier functions. The resulting classifier function tries to extract
   * an option or switch key from the current command line parameter using the
   * extractor function provided. If this succeeds, the key is passed to the
   * key classifier functions one by one until one returns a defined result.
   * This result is returned. If extraction of the key fails or none of the
   * classifier functions returns a result, an [[InputParameterElement]] is
   * returned.
   *
   * @param keyClassifiers a list of ''ExtractedKeyClassifierFunc'' functions
   * @param keyExtractor   the key extractor function
   * @return a ''CliClassifierFunc'' constructed from these parameters
   */
  def classifierOf(keyClassifiers: List[ExtractedKeyClassifierFunc])(keyExtractor: KeyExtractorFunc):
  CliClassifierFunc = {
    @tailrec
    def classifyKey(classifiers: List[ExtractedKeyClassifierFunc], key: ParameterKey, args: Seq[String], idx: Int):
    Option[CliElement] =
      classifiers match {
        case h :: t =>
          h(key, args, idx) match {
            case res@Some(_) => res
            case None => classifyKey(t, key, args, idx)
          }
        case _ => None
      }

    (args, idx) =>
      keyExtractor(args(idx)) flatMap (key => classifyKey(keyClassifiers,
        key, args, idx)) getOrElse InputParameterElement(args(idx), idx)
  }

  /**
   * Returns a key classifier func for options. The function checks whether the
   * passed in key references an option in the model context. If so, a
   * corresponding result is returned.
   *
   * @param modelContext the model context
   * @param resolverFunc function to resolve alias keys
   * @return the key classifier function for options
   */
  def optionKeyClassifierFunc(modelContext: => ModelContext)(resolverFunc: AliasResolverFunc):
  ExtractedKeyClassifierFunc = {
    lazy val context = modelContext
    (key, args, idx) =>
      if (getModelContextAttribute(context, key, ParameterModel.AttrParameterType,
        ParameterModel.ParameterTypeSwitch)(resolverFunc) == ParameterModel.ParameterTypeOption) {
        val value = args.lift(idx + 1)
        Some(OptionElement(key, value, idx))
      }
      else None
  }

  /**
   * Returns a key classifier function for switches. The function checks
   * whether the passed in key references a switch in the model context. If so,
   * a corresponding result is returned.
   *
   * @param modelContext the model context
   * @param resolverFunc function to resolve alias keys
   * @return the key classifier function for switches
   */
  def switchKeyClassifierFunc(modelContext: => ModelContext)(resolverFunc: AliasResolverFunc):
  ExtractedKeyClassifierFunc = {
    lazy val context = modelContext
    (key, _, index) =>
      classifySwitchKey(context, key, index)(resolverFunc)
  }

  /**
   * Returns a key classifier function for switches that can extract multiple
   * short alias keys. For long keys, the classifier function behaves in the
   * same way as the one returned by ''switchKeyClassifierFunc()''. For short
   * aliases, however, the function assumes that the key passed in consists of
   * multiple single letter key aliases and returns corresponding keys in the
   * resulting ''SwitchesElement''. Note that here no checks for the parameter
   * type are done. So to work correctly, the function should be executed after
   * a check for option elements.
   *
   * @param modelContext the model context
   * @param resolverFunc function to resolve alias keys
   * @return the key classifier function for multiple switches
   */
  def combinedSwitchKeyClassifierFunc(modelContext: => ModelContext)(resolverFunc: AliasResolverFunc):
  ExtractedKeyClassifierFunc = {
    lazy val context = modelContext
    (key, _, index) =>
      if (!key.shortAlias) classifySwitchKey(context, key, index)(resolverFunc)
      else {
        val switches = key.key.map { c =>
          val switchKey = ParameterKey(c.toString, shortAlias = true)
          val switchValue = getModelContextAttribute(modelContext, switchKey, ParameterModel.AttrSwitchValue,
            "true")(resolverFunc)
          (switchKey, switchValue)
        }.toList
        Some(SwitchesElement(switches, index))
      }
  }

  /**
   * Returns a ''FileOptionFunc'' that detects the given parameter keys as file
   * options. During file option processing, the command line elements are
   * classified. If an element is an option with a key in the given list, the
   * value of this option is returned as name of a parameter file.
   *
   * @param options a sequence with options referencing parameter files
   * @return the ''FileOptionFunc'' detecting these keys
   */
  def fileOptionFuncForOptions(options: Iterable[ParameterKey]): FileOptionFunc =
    fileOptionFuncForOptionsSet(options.toSet)

  /**
   * Processes the passed in sequence of parameters and resolves all parameter
   * files detected by the given ''FileOptionFunc''. The files are read, and
   * their content is added to the command line, replacing the original file
   * options. If successful, result is the full sequence of command line
   * parameters with all parameter files included.
   *
   * As I/O operations may fail, this function returns a ''Try''. In case of a
   * failure, the exception is of type [[ParameterFileException]] and contains
   * further information about the failed read operation.
   *
   * @param args           the sequence with command line arguments
   * @param classifierFunc a function to classify parameters
   * @param fileOptionFunc a function to detect file options
   * @return the final sequence of parameters including all parameter files
   */
  def processParameterFiles(args: Seq[String])(classifierFunc: CliClassifierFunc)(fileOptionFunc: FileOptionFunc):
  Try[Seq[String]] = {
    def processFileOptionsInArgs(args: Seq[String], processedFiles: Set[String]): Try[(Seq[String], Set[String])] = {
      val argList = args.toList
      val parameterFiles = classify(args)(classifierFunc)
        .map(elem => (elem.index, fileOptionFunc(elem)))
        .filter(_._2.isDefined)
        .map(t => (t._1, t._2.get))
      if (parameterFiles.isEmpty) Success((args, processedFiles))
      else {
        val init = Try((List.empty[String], args.length, processedFiles))
        val result = parameterFiles.foldLeft(init) { (triedState, file) =>
          triedState flatMap { state =>
            val (currentArgs, pos, knownFiles) = state
            val (fileKey, paramFile) = file._2
            val triedFileArgs = if (knownFiles contains paramFile) Success((List.empty[String], knownFiles))
            else handleParameterFileException(readParameterFile(paramFile), fileKey, paramFile)
              .flatMap(fileArgs => processFileOptionsInArgs(fileArgs, knownFiles + paramFile))
            triedFileArgs map { fileArgs =>
              val nextArgs = fileArgs._1.toList ::: argList.slice(file._1 + 2, pos) ::: currentArgs
              (nextArgs, file._1, knownFiles ++ fileArgs._2)
            }
          }
        }
        result map { state =>
          (argList.slice(0, state._2) ::: state._1, state._3)
        }
      }
    }

    processFileOptionsInArgs(args, Set.empty) map (_._1)
  }

  /**
   * Parses the command line arguments and converts them into a map keyed by
   * options. The parsing operation can be customized by specifying some
   * properties, especially a ''CliClassifierFunc''. This function is invoked
   * for each argument, and - based on its result - the parameter is added to
   * the result produced by this function. Note that the parsing operation
   * always succeeds, even if invalid parameters are passed in; this is
   * detected and handled later in the extraction phase.
   *
   * @param args              the sequence with command line arguments
   * @param classifierFunc    a function to classify parameters
   * @param aliasResolverFunc a function to resolve aliases
   * @return a ''Try'' with the parsed map of arguments
   */
  def parseParameters(args: Seq[String])
                     (classifierFunc: CliClassifierFunc)
                     (aliasResolverFunc: AliasResolverFunc): ParametersMap = {
    def appendOptionValue(argMap: InternalParamMap, key: ParameterKey, elem: CliElement): InternalParamMap = {
      val optValues = argMap.getOrElse(key, List.empty)
      argMap + (key -> (optValues :+ elem))
    }

    classify(args)(classifierFunc)
      .reverse
      .foldLeft(Map.empty[ParameterKey, List[CliElement]]) { (argsMap, elem) =>
        elem match {
          case elem: InputParameterElement =>
            appendOptionValue(argsMap, InputParameter, elem)

          case elem@OptionElement(key, optValue, _) =>
            optValue.fold(argsMap)(_ => appendOptionValue(argsMap,
              resolveAlias(key)(aliasResolverFunc), elem))

          case SwitchesElement(switches, index) =>
            switches.foldLeft(argsMap) { (map, t) =>
              appendOptionValue(map, resolveAlias(t._1)(aliasResolverFunc), OptionElement(t._1, Some(t._2), index))
            }
        }
      }
  }

  /**
   * Returns a ''FileOptionFunc'' that detects the parameter keys in the given
   * set as file options. This function has been extracted from
   * ''fileOptionFuncForOptions()'' to fix an IntelliJ warning.
   *
   * @param options a set with options referencing parameter files
   * @return the ''FileOptionFunc'' detecting these keys
   */
  private def fileOptionFuncForOptionsSet(options: Set[ParameterKey]): FileOptionFunc = {
    case OptionElement(key, value, _) if options.contains(key) =>
      value map (v => (key, v))
    case _ => None
  }

  /**
   * Reads a file with parameters and returns its single lines
   * as a list of strings.
   *
   * @param path the path to the parameters
   * @return a ''Try'' with the result of the read operation
   */
  private def readParameterFile(path: String): Try[List[String]] = Try {
    val source = Source.fromFile(path)
    source.getLines()
      .filter(_.nonEmpty)
      .toList
  }

  /**
   * Generates a meaningful exception when reading of a parameter file fails.
   *
   * @param readResult    the result of the read operation
   * @param fileOptionKey the key of the file option
   * @param file          the name of the file that was read
   * @return the updated read result
   */
  private def handleParameterFileException(readResult: Try[List[String]], fileOptionKey: ParameterKey, file: String):
  Try[List[String]] =
    readResult recoverWith {
      case e: Exception =>
        Failure(new ParameterFileException(s"Failed to load parameter file $file", e, fileOptionKey))
    }

  /**
   * Implements the classification for switches based on the key. Checks
   * whether the parameter with this key references a switch. If so, a
   * corresponding element is returned.
   *
   * @param context      the model context
   * @param key          the key of the switch
   * @param index        the index on the command line
   * @param resolverFunc function to resolve alias keys
   * @return an ''Option'' with the classified ''SwitchesElement''
   */
  private def classifySwitchKey(context: => ModelContext, key: ParameterKey, index: Int)
                               (resolverFunc: AliasResolverFunc): Option[SwitchesElement] =
    if (getModelContextAttribute(context, key, ParameterModel.AttrParameterType,
      ParameterModel.ParameterTypeOption)(resolverFunc) == ParameterModel.ParameterTypeSwitch)
      Some(SwitchesElement(List((key,
        getModelContextAttribute(context, key, ParameterModel.AttrSwitchValue, "true")(resolverFunc))), index))
    else None

  /**
   * Classifies all command line elements in the given sequence. Result is a
   * list with the elements found. This list is in reverse order based on the
   * positions.
   *
   * @param args           the sequence of arguments
   * @param classifierFunc the classifier function
   * @return a list with the classified elements
   */
  private def classify(args: Seq[String])(classifierFunc: CliClassifierFunc): List[CliElement] = {
    @tailrec def doClassify(index: Int, processed: List[CliElement]): List[CliElement] =
      if (index >= args.size) processed
      else {
        val element = classifierFunc(args, index)
        val updatedProcessed = element :: processed
        val increment = element match {
          case _: OptionElement => 2
          case _ => 1
        }
        doClassify(index + increment, updatedProcessed)
      }

    doClassify(0, Nil)
  }


  /**
   * Fetches a specific attribute from a ''ModelContext'' for a given parameter
   * key. If the key or the attribute cannot be found, the default value is
   * returned.
   *
   * @param context      the ''ModelContext''
   * @param key          the key of the parameter in question
   * @param attr         the desired attribute
   * @param default      the default value to use
   * @param resolverFunc function to resolve alias keys
   * @tparam A the data type of the attribute
   * @tparam B the data type of the default value
   * @return the value of this attribute (or the default)
   */
  private def getModelContextAttribute[A <: AnyRef, B <: A](context: ModelContext, key: ParameterKey,
                                                            attr: ParameterAttributeKey[A], default: => B)
                                                           (resolverFunc: AliasResolverFunc): A = {
    val resolvedKey = resolveAlias(key)(resolverFunc)
    context.options.get(resolvedKey)
      .flatMap(_.get(attr))
      .getOrElse(default)
  }

  /**
   * Resolves a potential parameter alias using the resolver function
   * specified. If no alias can be resolved, the key is returned as is.
   *
   * @param key          the key to be resolved
   * @param resolverFunc function to resolve alias keys
   * @return the resolved key
   */
  private def resolveAlias(key: ParameterKey)(resolverFunc: AliasResolverFunc): ParameterKey =
    resolverFunc(key) getOrElse key
}
