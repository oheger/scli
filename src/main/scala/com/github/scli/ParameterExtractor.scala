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

import java.nio.file.{Path, Paths}

import com.github.scli.ParameterModel.ModelContext
import com.github.scli.ParameterParser.ParametersMap

import scala.collection.SortedSet
import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

/**
 * A generic service responsible for processing an already parsed command line
 * and to extract configuration objects out of it.
 *
 * This service offers functionality to process the command line arguments
 * passed to an application and to convert them to specific configuration
 * objects. There are helper functions to interpret options of different types
 * and to collect arguments that are no options (such as files or directories
 * to be processed). It is possible to check whether all mandatory options are
 * present and that no unsupported options have been specified.
 *
 * This service operates on a map of parameters as produced by the
 * [[ParameterParser]] service. While the initial parsing step is pretty
 * simple, here the interpretation of command line options takes place. This
 * is done in form of [[com.github.scli.ParameterExtractor.CliExtractor]]
 * objects. A basic extractor obtains a single value from the command line.
 * The value can then be processed, e.g. checked or converted. Extractors can
 * be composed in monadic ways to construct more complex objects out of the
 * input values passed to the application.
 */
object ParameterExtractor {
  /** A mapping storing the boolean literals for conversion. */
  private final val BooleanMapping = Map("true" -> true, "false" -> false)

  /** Constant for an initial, empty model context. */
  private val EmptyModelContext = new ModelContext(Map.empty, SortedSet.empty, None, Nil)

  /**
   * A dummy parameter context object that is used if no current context is
   * available. It contains only dummy values.
   */
  private val DummyParameterContext = ParameterContext(Parameters(Map.empty, Set.empty),
    EmptyModelContext, DummyConsoleReader)

  /**
   * Type definition for the base type of a command line option. The option
   * can have an arbitrary type and multiple values. As each type conversion
   * can fail, it is a ''Try''.
   */
  type OptionValue[A] = Try[Iterable[A]]

  /**
   * Type definition for the value of an option that accepts a single value at
   * most. Parsing the option may cause an error if there are multiple values
   * (because the option key had been repeated); therefore a ''Try'' is used.
   * As the value may be undefined, there is also an ''Option'' included. Some
   * mapping extractors operate on this type.
   */
  type SingleOptionValue[A] = Try[Option[A]]

  /**
   * A data class storing the information required for extracting command
   * line options.
   *
   * This class is used by ''ParameterManager'' to represent parsed command
   * line arguments and to keep track about the option keys that have been
   * read by the application. (This is needed to find additional options
   * provided by the user that are not supported by the application.)
   *
   * @param parametersMap      the map with the options and their values
   * @param accessedParameters a set with the option keys that were queried
   */
  case class Parameters(parametersMap: ParametersMap, accessedParameters: Set[String]) {
    /**
     * Returns a new instance of ''Parameters'' that has the given key marked
     * as accessed.
     *
     * @param key the key affected
     * @return the updated ''Parameters'' instance
     */
    def keyAccessed(key: String): Parameters =
      if (accessedParameters contains key) this
      else copy(accessedParameters = accessedParameters + key)

    /**
     * Returns a new instance of ''Parameters'' that has the given keys marked
     * as accessed.
     *
     * @param keys the collection with keys affected
     * @return the updated ''Parameters'' instance
     */
    def keysAccessed(keys: Iterable[String]): Parameters =
      copy(accessedParameters = accessedParameters ++ keys)

    /**
     * Returns a flag whether all keys in the parameter maps have been
     * accessed. If this property is '''false''' at the end of command line
     * processing, this means that the command line contained unsupported
     * options.
     *
     * @return a flag whether all option keys have been accessed
     */
    def allKeysAccessed: Boolean =
      parametersMap.keySet.forall(accessedParameters.contains)

    /**
     * Returns a set with the option keys that are present, but have not been
     * accessed during command line processing.
     *
     * @return a set with the keys that have not been accessed
     */
    def notAccessedKeys: Set[String] = parametersMap.keySet -- accessedParameters
  }

  /**
   * A data class storing all the information required for the processing of
   * command line arguments.
   *
   * An instance of this class is passed to a ''CliExtractor'' when it is
   * executed. It stores the actual [[Parameters]] plus some helper objects
   * that may be needed to extract meaningful data or provide information
   * about the extractor.
   *
   * @param parameters  the parameters to be processed
   * @param helpContext the context to generate help information
   * @param reader      an object to read data from the console
   */
  case class ParameterContext(parameters: Parameters,
                              helpContext: ModelContext,
                              reader: ConsoleReader) {
    /**
     * Returns a new ''ParameterContext'' object that was updated with the
     * given ''Parameters'' and model context. All other properties remain
     * constant.
     *
     * @param nextParameters   the ''Parameters'' to replace the current ones
     * @param nextModelContext the updated model context
     * @return the updated ''ParameterContext''
     */
    def update(nextParameters: Parameters, nextModelContext: ModelContext): ParameterContext =
      copy(parameters = nextParameters, helpContext = nextModelContext)

    /**
     * Returns a new ''ParameterContext'' object with an updated
     * ''ModelContext'', to which the given attribute has been added.
     *
     * @param attr  the attribute key
     * @param value the attribute value
     * @return the updated ''ParameterContext''
     */
    def updateHelpContext(attr: String, value: String): ParameterContext =
      copy(helpContext = helpContext.addAttribute(attr, value))

    /**
     * Returns a ''ParameterContext'' for a conditional update of the
     * ''ModelContext''. If the passed in attribute value is defined, the
     * model context is replaced; otherwise, the same ''ParameterContext'' is
     * returned.
     *
     * @param attr     the attribute key
     * @param optValue an ''Option'' with the attribute value
     * @return the updated (or same) ''ParameterContext''
     */
    def updateHelpContextConditionally(attr: String, optValue: Option[String]): ParameterContext =
      optValue map (value => updateHelpContext(attr, value)) getOrElse this
  }

  /**
   * A data class to represent an error during parameter extraction.
   *
   * Instances of this class are generated by ''CliExtractor'' objects if
   * invalid parameters are detected. The properties contain all the
   * information available about the error.
   *
   * @param key     the key of the option with the invalid parameter
   * @param message an error message
   * @param context the current parameter context
   */
  case class ExtractionFailure(key: String,
                               message: String,
                               context: ParameterContext)

  object ParameterExtractionException {
    /**
     * Creates a new instance of ''ParameterExtractionException'' that stores
     * the given extraction failure.
     *
     * @param failure the ''ExtractionFailure'' to be recorded
     * @return the resulting exception instance
     */
    def apply(failure: ExtractionFailure): ParameterExtractionException =
      new ParameterExtractionException(List(failure))

    /**
     * Creates a new instance of ''ParameterExtractionException'' that stores
     * the given extraction failures. The passed in list must contain at least
     * one element; otherwise, an ''IllegalArgumentException'' exception is
     * thrown.
     *
     * @param failures the failures to add to the exception
     * @return the resulting exception instance
     */
    def apply(failures: List[ExtractionFailure]): ParameterExtractionException =
      if (failures.isEmpty)
        throw new IllegalArgumentException("Cannot create ParameterExtractionException without failures")
      else new ParameterExtractionException(failures)

    /**
     * Generates a string from all the given failures that is used as
     * exception message for a ''ParameterExtractionException''.
     *
     * @param failures the list with failures
     * @return the resulting message
     */
    private def generateExceptionMessage(failures: List[ExtractionFailure]): String =
      failures.map(f => s"${f.key}: ${f.message}")
        .mkString(", ")
  }

  /**
   * A special exception class used by ''CliExtractor'' objects to report
   * failures during parameter extraction.
   *
   * An instance can store multiple failures; so information about all invalid
   * options passed to the command line can be accumulated and displayed.
   *
   * @param failures a list with failures
   */
  class ParameterExtractionException private(val failures: List[ExtractionFailure])
    extends Exception(ParameterExtractionException.generateExceptionMessage(failures)) {
    /**
     * Returns a ''ParameterContext'' from the list of failures. (It is
     * unspecified, from which failure the context is obtained.)
     *
     * @return a ''ParameterContext'' from the failures managed by this object
     */
    def parameterContext: ParameterContext =
      failures.head.context
  }

  /**
   * A case class representing an extractor for command line options.
   *
   * This is a kind of state action. Such extractors can be combined to
   * extract multiple options from the command line and to mark the
   * corresponding option keys as accessed.
   *
   * @param run    a function to obtain an option and update the arguments map
   * @param optKey optional key of the option to be extracted
   * @tparam A the type of the result of the extractor
   */
  case class CliExtractor[A](run: ParameterContext => (A, ParameterContext), optKey: Option[String] = None) {
    /**
     * Returns the key of the option this extractor deals with. If there is no
     * key, result is an empty string. This case should normally not occur in
     * practice.
     *
     * @return the key of the option to be extracted by this extractor
     */
    def key: String = optKey getOrElse ""

    def flatMap[B](f: A => CliExtractor[B]): CliExtractor[B] = CliExtractor(ctx => {
      val (a, map1) = run(ctx)
      f(a).run(map1)
    }, optKey)

    def map[B](f: A => B): CliExtractor[B] =
      flatMap(a => CliExtractor(ctx => (f(a), ctx), optKey))

    /**
     * Returns a ''CliExtractor'' based on the current one that applies a
     * mapping function to the original result and also modifies the
     * parameter context.
     *
     * @param f the mapping function that is also passed the context
     * @tparam B the result type of the new extractor
     * @return the new ''CliExtractor''
     */
    def mapWithContext[B](f: (A, ParameterContext) => (B, ParameterContext)): CliExtractor[B] = {
      val fExt: A => CliExtractor[B] = a => {
        CliExtractor(ctx => f(a, ctx))
      }
      flatMap(fExt)
    }
  }

  /**
   * A class providing additional operations on a ''CliExtractor'' of type
   * ''OptionValue''.
   *
   * With the help of this class and an implicit conversion function the
   * construction of more complex ''CliExtractor'' objects is simplified.
   * Specific functionality can be added to an extractor by invoking one of
   * the functions offered by this class rather than using the functions of
   * [[ParameterExtractor]].
   *
   * @param ext the ''CliExtractor'' decorated by this class
   * @tparam A the result type of the ''CliExtractor''
   */
  class CliExtractorOptionsOps[A](ext: CliExtractor[OptionValue[A]]) {
    /**
     * Adds a fallback or default value to the managed ''CliExtractor''. If
     * the original extractor does not yield a value, the fallback extractor
     * is evaluated.
     *
     * @param fallbackExt the fallback ''CliExtractor''
     * @return the ''CliExtractor'' supporting a fallback
     */
    def fallback(fallbackExt: CliExtractor[OptionValue[A]]): CliExtractor[OptionValue[A]] =
      withFallback(ext, fallbackExt)

    /**
     * Adds an extractor as fallback to the managed ''CliExtractor'' that
     * produces the passed in constant values. Works the same way as
     * ''fallback()'', but creates the fallback extractor itself. A
     * description for the value is generated based on the passed in
     * parameters.
     *
     * @param firstValue the first fallback value
     * @param moreValues additional fallback values
     * @return the ''CliExtractor'' supporting these fallback values
     */
    def fallbackValues(firstValue: A, moreValues: A*): CliExtractor[OptionValue[A]] =
      fallback(constantOptionValue(firstValue, moreValues: _*))

    /**
     * Adds an extractor as fallback to the managed ''CliExtractor'' that
     * produces the passed in constant values and sets the value description
     * specified. Works the same way as ''fallbackValues()'', but allows more
     * control over the value description.
     *
     * @param optValueDesc the optional description of the value
     * @param firstValue   the first fallback value
     * @param moreValues   additional fallback values
     * @return the ''CliExtractor'' supporting these fallback values
     */
    def fallbackValuesWithDesc(optValueDesc: Option[String], firstValue: A, moreValues: A*):
    CliExtractor[OptionValue[A]] =
      fallback(constantOptionValueWithDesc(optValueDesc, firstValue, moreValues: _*))

    /**
     * Returns a ''CliExtractor'' based on the managed extractor that yields a
     * single optional value. So the option extracted by this extractor may
     * have at most one value.
     *
     * @return the ''CliExtractor'' yielding a single optional value
     */
    def single: CliExtractor[SingleOptionValue[A]] =
      asSingleOptionValue(ext)

    /**
     * Returns a ''CliExtractor'' that checks whether the number of values
     * passed to the current option corresponds to the multiplicity specified
     * by this function. If too few or too many values are provided, the
     * extractor fails with a corresponding exception.
     *
     * @param atLeast the minimum number of values
     * @param atMost  the maximum number of values (a value less than 0 means
     *                that there is no restriction)
     * @return the ''CliExtractor'' enforcing the multiplicity
     */
    def multiplicity(atLeast: Int = 0, atMost: Int = -1): CliExtractor[OptionValue[A]] =
      withMultiplicity(ext, atLeast, atMost)

    /**
     * Returns a ''CliExtractor'' that yields a boolean result indicating
     * whether the option extracted by the managed extractor has a value. This
     * is useful for instance when constructing conditional extractors; so
     * conditions can be defined based on the presence of certain options.
     *
     * @return the ''CliExtractor'' checking whether an option has a value
     */
    def isDefined: CliExtractor[Try[Boolean]] = isOptionDefined(ext)

    /**
     * Returns a ''CliExtractor'' that yields the value of the managed
     * extractor with the given mapping function applied to it. In contrast to
     * the plain ''map()'' function, this function is more convenient for
     * values of type ''OptionValue'' because the mapping function operates
     * directly on the values and does not have to deal with ''Try'' or
     * ''Iterable'' objects.
     *
     * @param f the mapping function on the option values
     * @tparam B the result type of the mapping function
     * @return the ''CliExtractor'' applying the mapping function
     */
    def mapTo[B](f: A => B): CliExtractor[OptionValue[B]] =
      mapped(ext)(f)
  }

  /**
   * A trait providing additional functionality to ''CliExtractor'' objects
   * related to data type conversions.
   *
   * This trait offers some functions to convert string option values of a
   * generic option value type to other data types. If a conversion fails
   * (because the input string has an unexpected format), the resulting
   * ''CliExtractor'' yields a ''Failure'' result.
   *
   * The data type conversions are implemented based on the abstract ''mapExt''
   * function, which has to be implemented in concrete sub classes in a way
   * suitable for the option value type supported.
   *
   * @tparam F the option value type of the wrapped ''CliExtractor''
   */
  trait CliExtractorConversions[F[_]] {
    /**
     * Returns the wrapped ''CliExtractor'', on which data type conversions are
     * implemented.
     *
     * @return the wrapped ''CliExtractor''
     */
    def ext: CliExtractor[F[String]]

    /**
     * Returns a ''CliExtractor'' that converts the option values of the
     * managed extractor to ''Int'' values.
     *
     * @return the ''CliExtractor'' extracting ''Int'' values
     */
    def toInt: CliExtractor[F[Int]] = mapExt(_.toInt)

    /**
     * Returns a ''CliExtractor'' that converts the option values of the
     * managed extractor to ''Boolean'' values. The strings must have the
     * values *true* or *false* to be recognized. Note that case matters; if
     * the conversion should be case insensitive, convert the values to lower
     * case before (by applying the ''toLower'' conversion).
     *
     * @return the ''CliExtractor'' extracting ''Boolean'' values
     */
    def toBoolean: CliExtractor[F[Boolean]] =
      toEnum(BooleanMapping.get)

    /**
     * Returns a ''CliExtractor'' that converts the option values of the
     * managed extractor to ''Path'' values.
     *
     * @return the ''CliExtractor'' extracting ''Path'' values
     */
    def toPath: CliExtractor[F[Path]] = mapExt(s => Paths get s)

    /**
     * Returns a ''CliExtractor'' that interprets the values of this extractor
     * as enum literals by applying the given mapping function. If the mapping
     * function yields a result for a current value, the result becomes the
     * new value; otherwise, the extractor fails with an error message.
     *
     * @param fMap the enum mapping function
     * @tparam B the result type of the mapping function
     * @return the ''CliExtractor'' returning enum values
     */
    def toEnum[B](fMap: String => Option[B]): CliExtractor[F[B]] =
      mapExt { res =>
        fMap(res) match {
          case Some(value) => value
          case None => throw new IllegalArgumentException(s"Invalid enum value: $res")
        }
      }

    /**
     * Returns a string-based ''CliExtractor'' that returns values converted
     * to lower case.
     *
     * @return the ''CliExtractor'' returning lower case strings
     */
    def toLower: CliExtractor[F[String]] = mapExt(toLowerCase)

    /**
     * Returns a string-based ''CliExtractor'' that returns values converted
     * to upper case.
     *
     * @return the ''CliExtractor'' returning upper case strings
     */
    def toUpper: CliExtractor[F[String]] = mapExt(toUpperCase)

    /**
     * Implements a mechanism to apply a mapping function to the concrete
     * option value type supported by the wrapped ''CliExtractor''.
     *
     * @param f the mapping function to be applied
     * @tparam B the result type of the mapping function
     * @return a ''CliExtractor'' that applies the mapping function
     */
    protected def mapExt[B](f: String => B): CliExtractor[F[B]]
  }

  /**
   * A helper class providing additional functionality to ''CliExtractor''
   * objects operating on the ''OptionValue'' type related to data type
   * conversions.
   *
   * @param ext the ''CliExtractor'' decorated by this class
   */
  class CliExtractorConvertOps(override val ext: CliExtractor[OptionValue[String]])
    extends CliExtractorConversions[OptionValue] {
    override protected def mapExt[B](f: String => B): CliExtractor[OptionValue[B]] = mapped(ext)(f)
  }

  /**
   * A helper class providing additional functionality to ''CliExtractor''
   * objects that yield only a single value.
   *
   * @param ext the ''CliExtractor'' decorated by this class
   * @tparam A the result type of the ''CliExtractor''
   */
  class CliExtractorSingleOps[A](ext: CliExtractor[SingleOptionValue[A]]) {
    /**
     * Returns a ''CliExtractor'' based on the managed extractor that yields
     * a single value. Per default, ''CliExtractor'' objects of type
     * ''SingleOptionValue'' return an ''Option''. This function checks
     * whether the ''Option'' is defined. If so, its value is returned
     * directly; otherwise, the extractor yields a ''Failure'' result.
     *
     * @return the ''CliExtractor'' extracting a mandatory single value
     */
    def mandatory: CliExtractor[Try[A]] = asMandatory(ext)

    /**
     * Adds a fallback or default extractor to the managed ''CliExtractor''. If
     * the original extractor does not yield a value, the fallback extractor
     * is evaluated.
     *
     * @param fallbackExt the fallback ''CliExtractor''
     * @return the ''CliExtractor'' supporting a fallback
     */
    def fallback(fallbackExt: CliExtractor[SingleOptionValue[A]]): CliExtractor[SingleOptionValue[A]] =
      withFallbackSingle(ext, fallbackExt)

    /**
     * Adds an extractor as fallback to the managed ''CliExtractor'' that
     * produces the passed in constant value. Works the same way as
     * ''fallback()'', but creates the fallback extractor itself. A
     * description for the value is generated based on the passed in
     * parameter value.
     *
     * @param value the fallback value
     * @return the ''CliExtractor'' supporting these fallback values
     */
    def fallbackValue(value: A): CliExtractor[SingleOptionValue[A]] =
      fallback(constantOptionValue(value).single)

    /**
     * Adds an extractor as fallback to the managed ''CliExtractor'' that
     * produces the passed in constant value and sets the value description
     * specified. Works the same way as ''fallbackValue()'', but allows more
     * control over the value description.
     *
     * @param optValueDesc the optional description of the value
     * @param value        the fallback value
     * @return the ''CliExtractor'' supporting these fallback values
     */
    def fallbackValueWithDesc(optValueDesc: Option[String], value: A): CliExtractor[SingleOptionValue[A]] =
      fallback(constantOptionValueWithDesc(optValueDesc, value).single)

    /**
     * Returns a ''CliExtractor'' that yields the value of the managed
     * extractor with the given mapping function applied to it. In contrast to
     * the plain ''map()'' function, this function is more convenient for
     * values of type ''SingleOptionValue'' because the mapping function
     * operates directly on the value (if it is present) and does not have to
     * deal with ''Try'' or ''Option'' objects.
     *
     * @param f the mapping function on the option value
     * @tparam B the result type of the mapping function
     * @return the ''CliExtractor'' applying the mapping function
     */
    def mapTo[B](f: A => B): CliExtractor[SingleOptionValue[B]] = mappedSingle(ext)(f)

    /**
     * Returns a ''CliExtractor'' that yields a boolean result indicating
     * whether the option extracted by the managed extractor has a value. This
     * is useful for instance when constructing conditional extractors; so
     * conditions can be defined based on the presence of certain options.
     *
     * @return the ''CliExtractor'' checking whether an option has a value
     */
    def isDefined: CliExtractor[Try[Boolean]] = isSingleOptionDefined(ext)
  }

  /**
   * A helper class providing additional functionality to ''CliExtractor''
   * objects operating on the ''SingleOptionValue'' type related to data type
   * conversions.
   *
   * @param ext the ''CliExtractor'' decorated by this class
   */
  class CliExtractorSingleConvertOps(override val ext: CliExtractor[SingleOptionValue[String]])
    extends CliExtractorConversions[SingleOptionValue] {
    override protected def mapExt[B](f: String => B): CliExtractor[SingleOptionValue[B]] = mappedSingle(ext)(f)
  }

  /**
   * An implicit conversion to create a ''Parameters'' object from a map of
   * parsed command line options.
   *
   * @param map the map
   * @return the resulting ''Parameters''
   */
  implicit def mapToParameters(map: ParametersMap): Parameters =
    Parameters(map, Set.empty)

  /**
   * An implicit conversion function to decorate a ''CliExtractor'' with a
   * ''CliExtractorOptionsOps'' object.
   *
   * @param ext the extractor to be decorated
   * @tparam A the result type of the extractor
   * @return the ''CliExtractorOptionsOps'' object decorating this extractor
   */
  implicit def toOptionsOps[A](ext: CliExtractor[OptionValue[A]]): CliExtractorOptionsOps[A] =
    new CliExtractorOptionsOps(ext)

  /**
   * An implicit conversion function to decorate a ''CliExtractor'' with a
   * ''CliExtractorConvertOps'' object.
   *
   * @param ext the extractor to be decorated
   * @return the ''CliExtractorConvertOps'' object decorating this extractor
   */
  implicit def toConvertOps(ext: CliExtractor[OptionValue[String]]): CliExtractorConvertOps =
    new CliExtractorConvertOps(ext)

  /**
   * An implicit conversion function to decorate a ''CliExtractor'' with a
   * ''CliExtractorSingleConvertOps'' object.
   *
   * @param ext the extractor to be decorated
   * @return the ''CliExtractorSingleConvertOps'' object decorating this
   *         extractor
   */
  implicit def toSingleConvertOps(ext: CliExtractor[SingleOptionValue[String]]): CliExtractorSingleConvertOps =
    new CliExtractorSingleConvertOps(ext)

  /**
   * An implicit conversion function to decorate a ''CliExtractor'' with a
   * ''CliExtractorSingleOps'' object.
   *
   * @param ext the extractor to be decorated
   * @tparam A the result type of the extractor
   * @return the ''CliExtractorSingleOps'' object decorating this extractor
   */
  implicit def toSingleOps[A](ext: CliExtractor[SingleOptionValue[A]]): CliExtractorSingleOps[A] =
    new CliExtractorSingleOps(ext)

  /**
   * Returns an option value of the given type that does not contain any data.
   * This is used by some extractors to set default values that are not
   * further evaluated.
   *
   * @return the empty option value of the given type
   * @tparam A the type of the option value
   */
  def emptyOptionValue[A]: OptionValue[A] = Success(List.empty[A])

  /**
   * Returns a ''CliExtractor'' that always produces an empty value. This
   * is useful in some cases, e.g. to define an extractor when one is
   * required, but the concrete value does not matter.
   *
   * @return the ''CliExtractor'' producing empty values
   * @tparam A the type of the option value
   */
  def emptyExtractor[A]: CliExtractor[OptionValue[A]] = constantExtractor(emptyOptionValue)

  /**
   * Returns a ''CliExtractor'' that always returns the given constant value
   * as result without manipulating the parameter context. This extractor is
   * mainly useful for building up complex extractors, e.g. together with
   * conditions or default values for optional parameters.
   *
   * @param a            the constant value to be returned
   * @param optValueDesc an optional description of the default value
   * @tparam A the type of the value
   * @return the ''CliExtractor'' returning this constant value
   */
  def constantExtractor[A](a: A, optValueDesc: Option[String] = None): CliExtractor[A] =
    CliExtractor(context => {
      val nextContext = context.updateHelpContextConditionally(ParameterModel.AttrFallbackValue, optValueDesc)
      (a, nextContext)
    })

  /**
   * Returns a ''CliExtractor'' that returns a constant collection of option
   * values of the given type. This is a special case of a constant extractor
   * that operates on the base type of command line arguments. This function
   * automatically generates a description of the default value (based on the
   * values passed in). Use the ''constantOptionValueWithDesc()'' function to
   * define a description manually.
   *
   * @param first the first value
   * @param items a sequence of additional values
   * @return the ''CliExtractor'' returning this constant ''OptionValue''
   * @tparam A the type of the resulting option value
   */
  def constantOptionValue[A](first: A, items: A*): CliExtractor[OptionValue[A]] = {
    val values = first :: items.toList
    val valueDesc = generateValueDescription(values)
    constantExtractor(Success(values), Some(valueDesc))
  }

  /**
   * Returns a ''CliExtractor'' that returns a constant collection of option
   * values of the given type and sets the given value description. This
   * function works like ''constantOptionValue()'', but offers more
   * flexibility regarding the description of the value.
   *
   * @param optValueDesc an ''Option'' with the value description; ''None'' to
   *                     set no description
   * @param first        the first value
   * @param items        a sequence of additional values
   * @tparam A the type of the resulting option value
   * @return the ''CliExtractor'' returning this constant ''OptionValue''
   */
  def constantOptionValueWithDesc[A](optValueDesc: Option[String], first: A, items: A*): CliExtractor[OptionValue[A]] =
    constantExtractor(Success(first :: items.toList), optValueDesc)

  /**
   * Returns an extractor that extracts the value of the specified option key
   * in its basic string representation. If it checked whether this option has
   * at most one value; if multiple values are found, the extractor fails.
   *
   * @param key  the key of the option
   * @param help an optional help text for this option
   * @return the extractor to extract the option value
   */
  def optionValue(key: String, help: Option[String] = None): CliExtractor[SingleOptionValue[String]] =
    asSingleOptionValue(multiOptionValue(key, help))

  /**
   * Returns an extractor that extracts all values of the specified option key
   * in their basic string representation.
   *
   * @param key  the key of the option
   * @param help an optional help text for this option
   * @return the extractor to extract the option values
   */
  def multiOptionValue(key: String, help: Option[String] = None): CliExtractor[OptionValue[String]] =
    CliExtractor(context => {
      val values = context.parameters.parametersMap.getOrElse(key, Nil)
      val nextHelpCtx = context.helpContext.addOption(key, help)
      (Success(values), context.update(context.parameters keyAccessed key, nextHelpCtx))
    }, Some(key))

  /**
   * Returns an extractor that extracts a value from the input parameters. An
   * input parameter is a parameter that is not the value of an option. The
   * position of the parameter in the command line needs to be specified. The
   * position starts with index 0. It can be negative to count from the end of
   * the command line; for instance, index -1 represents the last input
   * parameter, index -2 the one before the last, etc. If no parameter with
   * this index exists (because the user has entered too few parameters), the
   * extractor generates a failure. It is also possible to check whether too
   * many parameters have been provided. This is done by setting the ''last''
   * flag to '''true''' for the input parameter with the maximum index. (This
   * works only if positive index values are used.) The extractor then
   * generates a failure if more input values are defined.
   *
   * To support the generation of usage texts, a key and a help text can be
   * assigned to the input parameter. The key can be used in the overview of
   * the command line; the help text is a more detailed description of this
   * parameter.
   *
   * @param index   the index of the input parameter to be extracted
   * @param optKey  an optional key to be assigned to this parameter
   * @param optHelp an optional help text
   * @param last    flag whether this is the last input parameter
   * @return the extractor to extract this input value
   */
  def inputValue(index: Int, optKey: Option[String] = None, optHelp: Option[String] = None, last: Boolean = false):
  CliExtractor[SingleOptionValue[String]] =
    inputValues(index, index, optKey, optHelp, last).single

  /**
   * Returns an extractor that extracts a sequence of values from the input
   * parameters. This function is similar to ''inputValue()'', but the result
   * can have multiple values; it is specified by the first and the last index
   * in the sequence of input parameters. Like for ''inputValue()'', indices
   * are 0-based and can be negative. For instance, by setting ''fromIdx''
   * to 1 and ''toIdx'' to -1, all values except for the first one are
   * extracted.
   *
   * @param fromIdx the start index of the input parameter
   * @param toIdx   the last index of the input parameter
   * @param optKey  an optional key to be assigned to this parameter
   * @param optHelp an optional help text
   * @param last    flag whether this is the last input parameter
   * @return the extractor to extract these input values
   */
  def inputValues(fromIdx: Int, toIdx: Int, optKey: Option[String] = None, optHelp: Option[String] = None,
                  last: Boolean = false): CliExtractor[OptionValue[String]] =
    CliExtractor(context => {
      val inputs = context.parameters.parametersMap.getOrElse(ParameterParser.InputOption, Nil)

      // handles special negative index values and checks the index range
      def adjustAndCheckIndex(index: Int): Try[Int] = {
        val adjustedIndex = if (index < 0) inputs.size + index
        else index
        if (adjustedIndex >= 0 && adjustedIndex < inputs.size) Success(adjustedIndex)
        else Failure(paramException(context, ParameterParser.InputOption, tooFewErrorText(adjustedIndex)))
      }

      def tooFewErrorText(index: Int): String = {
        val details = optKey map (k => s"'$k''") getOrElse s"for index $index"
        s"Too few input arguments; undefined argument $details."
      }

      val result = if (last && inputs.size > toIdx + 1)
        Failure(paramException(context, ParameterParser.InputOption,
          s"Too many input arguments; expected at most ${toIdx + 1}"))
      else
        for {
          firstIndex <- adjustAndCheckIndex(fromIdx)
          lastIndex <- adjustAndCheckIndex(toIdx)
        } yield inputs.slice(firstIndex, lastIndex + 1)
      val helpContext = context.helpContext.addInputParameter(fromIdx, optKey, optHelp)
      (result, context.update(context.parameters keyAccessed ParameterParser.InputOption, helpContext))
    }, optKey)

  /**
   * Returns an extractor that can apply a fallback (or default) value to
   * another extractor. The resulting extractor invokes the first extractor.
   * If this yields a defined result, this result is returned. Otherwise, the
   * fallback extractor is returned.
   *
   * @param ext         the first extractor to be invoked
   * @param fallbackExt the fallback extractor
   * @return the resulting extractor applying a fallback value
   * @tparam A the type of the option values
   */
  def withFallback[A](ext: CliExtractor[OptionValue[A]], fallbackExt: CliExtractor[OptionValue[A]]):
  CliExtractor[OptionValue[A]] =
    conditionalValue(ext.isDefined, ext, fallbackExt)

  /**
   * Returns an extractor that can apply a fallback (or default) value to
   * another extractor yielding a single value. This is analogous to
   * ''withFallback()'', but for the value type ''SingleOptionValue''.
   *
   * @param ext         the extractor to be decorated
   * @param fallbackExt the fallback extractor
   * @tparam A the type of the values
   * @return the resulting extractor applying a fallback value
   */
  def withFallbackSingle[A](ext: CliExtractor[SingleOptionValue[A]], fallbackExt: CliExtractor[SingleOptionValue[A]]):
  CliExtractor[SingleOptionValue[A]] =
    conditionalValue(ext.isDefined, ext, fallbackExt)

  /**
   * Returns an extractor that prompts the user for entering the value of an
   * option. This is done by delegating to the [[ConsoleReader]] in the
   * parameter context passed to the extractor. This function can be used for
   * instance together with ''withFallback()'' to let the user enter a value
   * if it has not been provided on the command line.
   *
   * @param key       the key of the option
   * @param password  a flag whether a password is to be entered
   * @param optPrompt an optional text to be displayed to the user; if
   *                  undefined, the option key is used as prompt
   * @return the extractor that reads from the console
   */
  def consoleReaderValue(key: String, password: Boolean = true, optPrompt: Option[String] = None):
  CliExtractor[SingleOptionValue[String]] =
    CliExtractor(context => {
      val prompt = optPrompt getOrElse key
      val consoleValue = context.reader.readOption(prompt, password)
      (Try(Some(consoleValue)), context)
    }, Some(key))

  /**
   * Returns an extractor that conditionally delegates to other extractors.
   * The condition is modelled as ''CliExtractor'' of type ''Try[Boolean]''.
   * This is because a typical use case is to extract one or multiple other
   * command line options and evaluate their values. If this extractor yields
   * '''true''', the ''ifExt'' extractor is executed. If the condition
   * extractor yields '''false''', the ''elseExt'' is executed. In case of a
   * failure, this extractor returns a failure with the same exception.
   *
   * Using this function, it is possible to implement quite complex scenarios.
   * For instance, a program can expect a ''mode'' parameter, and depending on
   * the concrete mode, a number of other parameters become enabled or
   * disabled.
   *
   * Each extractor can be assigned a group name; the options extracted by the
   * extractors are also associated with this group. When generating help
   * information for the CLI it is then possible to show only help texts for
   * options belonging to specific groups or to indicate that some options are
   * valid only under specific conditions.
   *
   * @param condExt   the extractor that defines the condition
   * @param ifExt     the extractor to run if the condition is fulfilled
   * @param elseExt   the extractor to run if the condition is not fulfilled
   * @param ifGroup   name of the group for the if extractor
   * @param elseGroup name of the group for the else extractor
   * @return the conditional extractor
   * @tparam A the type of the option values
   */
  def conditionalValue[A](condExt: CliExtractor[Try[Boolean]], ifExt: CliExtractor[Try[A]],
                          elseExt: CliExtractor[Try[A]], ifGroup: Option[String] = None,
                          elseGroup: Option[String] = None): CliExtractor[Try[A]] =
    CliExtractor(context => {
      val (condResult, context2) = condExt.run(context)
      condResult match {
        case Success(value) =>
          val (activeExt, activeGroup) = if (value) (ifExt, ifGroup) else (elseExt, elseGroup)
          val extractorsAndGroups = List((ifExt, ifGroup), (elseExt, elseGroup))
            .filter(_._1 != activeExt)
          val helpContext = updateHelpContext(context2.helpContext, extractorsAndGroups)
          val (result, context3) =
            activeExt.run(context2.copy(helpContext = helpContext startGroupConditionally activeGroup))
          (result, context3.copy(helpContext = context3.helpContext.endGroupConditionally(activeGroup)))

        case Failure(exception) =>
          (Failure(exception), context2)
      }
    })

  /**
   * A specialized variant of a conditional extractor that operates on sub
   * extractors of type ''OptionValue''. The main difference between this
   * function and ''conditionalValue()'' is that it is not necessary to provide
   * an extractor for the else case; a default extractor is used that returns
   * an empty value.
   *
   * @param condExt   the extractor that defines the condition
   * @param ifExt     the extractor to run if the condition is fulfilled
   * @param elseExt   the extractor to run if the condition is not fulfilled
   * @param ifGroup   name of the group for the if extractor
   * @param elseGroup name of the group for the else extractor
   * @return the conditional extractor
   * @tparam A the type of the option values
   */
  def conditionalOptionValue[A](condExt: CliExtractor[Try[Boolean]], ifExt: CliExtractor[OptionValue[A]],
                                elseExt: CliExtractor[OptionValue[A]] = emptyExtractor[A],
                                ifGroup: Option[String] = None,
                                elseGroup: Option[String] = None): CliExtractor[OptionValue[A]] =
    conditionalValue(condExt, ifExt, elseExt, ifGroup, elseGroup)

  /**
   * Returns an extractor that dispatches from the result of one extractor to
   * a group of other extractors. The original extractor yields a string value
   * which is looked up in a map to find the extractor to be executed.
   *
   * This extractor is useful for applications that support a mode or command
   * argument. Based on this argument, different command line options are
   * enabled or disabled. The map to be passed to this function in such a
   * scenario has the supported command names as strings and the extractors
   * querying the command-specific options as values. If the value returned
   * by the selector extractor is not found in the map, the resulting
   * extractor fails with an error message. It also fails if the selector
   * extractor fails (with the same exception).
   *
   * The keys in the map are also used as group names when invoking the
   * group-specific extractors. So the group-specific command line options can
   * be categorized and displayed in group-specific sections in the
   * application's help text.
   *
   * @param groupExt the ''CliExtractor'' that selects the active group
   * @param groupMap a map with extractors for the supported groups
   * @tparam A the result type of the resulting extractor
   * @return the extractor returning the group value
   */
  def conditionalGroupValue[A](groupExt: CliExtractor[Try[String]],
                               groupMap: Map[String, CliExtractor[Try[A]]]): CliExtractor[Try[A]] =
    CliExtractor(context => {
      val (triedGroup, context2) = groupExt.run(context)
      triedGroup match {
        case Success(group) =>
          groupMap.get(group).
            fold((Try[A](throw paramException(context, groupExt.key, s"Cannot resolve group '$group''")),
              context2)) { ext =>
              val extractorsAndGroups = groupMap.toList.filter(entry => entry._2 != ext)
                .map(entry => (entry._2, Some(entry._1)))
              val helpContext = updateHelpContext(context2.helpContext, extractorsAndGroups)
              val (result, context3) = ext.run(context2.copy(helpContext = helpContext startGroup group))
              (result, context3.copy(helpContext = context3.helpContext.endGroup()))
            }
        case Failure(exception) =>
          (Failure(exception), context2)
      }
    })

  /**
   * Returns an extractor that yields a flag whether the ''CliExtractor''
   * passed in extracts a defined value. The resulting extractor yields
   * '''true''' if the referenced extractor is successful and has at least one
   * option value.
   *
   * @param ext the extractor to be checked
   * @tparam A the type of the option values
   * @return the extractor checking whether there is a defined value
   */
  def isOptionDefined[A](ext: CliExtractor[OptionValue[A]]): CliExtractor[Try[Boolean]] =
    ext map { optionValue =>
      optionValue map (_.nonEmpty)
    }

  /**
   * Returns an extractor that yields a flag whether the ''CliExtractor''
   * operating on a ''SingleOptionValue'' passed in extracts a defined value.
   * This is analogous to ''isOptionDefined()'', but for extractors yielding
   * only a single value.
   *
   * @param ext the extractor to be checked
   * @tparam A the type of the option value
   * @return the extractor checking whether there is a defined value
   */
  def isSingleOptionDefined[A](ext: CliExtractor[SingleOptionValue[A]]): CliExtractor[Try[Boolean]] =
    ext map { triedValue =>
      triedValue map (_.nonEmpty)
    }

  /**
   * Returns an extractor that yields a flag whether the command line option
   * with the given key is defined. This is useful for instance to define
   * a condition for the ''conditionalValue'' extractor.
   *
   * @param key the key of the option in question
   * @return an extractor checking whether this option is defined
   */
  def isDefinedExtractor(key: String): CliExtractor[Try[Boolean]] =
    isOptionDefined(multiOptionValue(key))

  /**
   * Returns an extractor that extracts a single option value from the result
   * of the given extractor. It is an error if the result contains multiple
   * values; however, an undefined value is accepted.
   *
   * @param ext the extractor to be decorated
   * @return the extractor extracting the single option value
   */
  def asSingleOptionValue[A](ext: CliExtractor[OptionValue[A]]): CliExtractor[SingleOptionValue[A]] =
    ext.mapWithContext((optionValue, context) => {
      val res = optionValue flatMap { values =>
        if (values.size > 1)
          Failure(paramException(context, ext.key,
            s"should have a single value, but has multiple values - $optionValue"))
        else Success(values.headOption)
      }
      (res, context.updateHelpContext(ParameterModel.AttrMultiplicity, "0..1"))
    })

  /**
   * Returns an extractor that enforces an option to have a defined value. If
   * the provided extractor yields a ''Try'' with an undefined option, a
   * failure is generated. Otherwise, the option value is unwrapped.
   *
   * @param ext the extractor providing the original value
   * @tparam A the result type
   * @return the extractor returning a mandatory value
   */
  def asMandatory[A](ext: CliExtractor[SingleOptionValue[A]]): CliExtractor[Try[A]] =
    ext.mapWithContext((optionValue, context) => {
      val res = optionValue.flatMap {
        case Some(v) => Success(v)
        case None => Failure(paramException(context, ext.key, "mandatory option has no value"))
      }
      (res, context.updateHelpContext(ParameterModel.AttrMultiplicity, "1..1"))
    })

  /**
   * Returns an extractor that enforces the given multiplicity for the values
   * assigned to the current option. A failure is generated if too few or too
   * many values have been provided. Otherwise, no changes on the values are
   * made.
   *
   * @param ext     the extractor providing the original value
   * @param atLeast the minimum number of values
   * @param atMost  the maximum number of values (less than 0 for unlimited)
   * @tparam A the result type
   * @return the extractor checking the multiplicity
   */
  def withMultiplicity[A](ext: CliExtractor[OptionValue[A]], atLeast: Int, atMost: Int):
  CliExtractor[OptionValue[A]] =
    ext.mapWithContext((optionValue, context) => {
      val res = optionValue.flatMap { values =>
        if (values.size < atLeast)
          Failure(paramException(context, ext.key, s"option must have at least $atLeast values"))
        else if (atMost >= 0 && values.size > atMost)
          Failure(paramException(context, ext.key, s"option must have at most $atMost values"))
        else Success(values)
      }
      (res, context.updateHelpContext(ParameterModel.AttrMultiplicity,
        Multiplicity(atLeast, atMost).toString))
    })

  /**
   * Returns an extractor that modifies the result of another extractor by
   * applying a mapping function. While mapping is supported by extractors in
   * general, this function simplifies this for ''OptionValue'' objects.
   * The mapping function operates on the values collection, and it is called
   * only if the ''Try'' is  successful. The mapping function can throw an
   * exception; this is handled automatically by causing the result to fail.
   *
   * @param ext the extractor to be decorated
   * @param f   the mapping function to be applied
   * @tparam A the original result type
   * @tparam B the mapped result type
   * @return the extractor applying the mapping function
   */
  def mapped[A, B](ext: CliExtractor[OptionValue[A]])(f: A => B): CliExtractor[OptionValue[B]] =
    mappedWithContext(ext) { (a, context) =>
      (f(a), context)
    }

  /**
   * Returns an extractor that modifies the result of another extractor by
   * applying a mapping function that has access to the current
   * ''ParameterContext''. This function is analogous to ''mapped()'', but it
   * expects a mapping function that is passed in a ''ParameterContext'' and
   * returns an updated one.
   *
   * @param ext the extractor to be decorated
   * @param f   the mapping function to be applied
   * @tparam A the original result type
   * @tparam B the mapped result type
   * @return the extractor applying the mapping function
   */
  def mappedWithContext[A, B](ext: CliExtractor[OptionValue[A]])(f: (A, ParameterContext) => (B, ParameterContext)):
  CliExtractor[OptionValue[B]] =
    ext.mapWithContext { (triedResult, context) => {
      val mappedResult = triedResult.map(o => {
        val mappingResult = o.foldRight((context, List.empty[B], List.empty[ExtractionFailure])) { (a, t) =>
          paramTry(t._1, ext.key)(f(a, t._1)) match {
            case Success((b, nextCtx)) =>
              (nextCtx, b :: t._2, t._3)
            case f@Failure(_) =>
              (t._1, t._2, collectErrorMessages(f) ::: t._3)
          }
        }
        if (mappingResult._3.nonEmpty) {
          val errMsg = mappingResult._3.map(_.message).mkString(", ")
          (Failure[List[B]](ParameterExtractionException(mappingResult._3.head.copy(message = errMsg))), context)
        } else (Success(mappingResult._2), mappingResult._1)
      })

      mappedResult match {
        case Success(res) => res
        case Failure(exception) =>
          (Failure(exception), context)
      }
    }
    }

  /**
   * Returns an extractor for a ''SingleOptionValue'' that modifies another
   * extractor by applying a mapping function. This function corresponds to the
   * ''mapped()'' function for option values of type ''SingleOptionValue''; the
   * mapping function can focus on data values and does not have to deal with
   * the wrapping ''Try'' or ''Option''.
   *
   * @param ext the extractor to be decorated
   * @param f   the mapping function
   * @tparam A the data type of the original extractor
   * @tparam B the data type of the resulting extractor
   * @return the extractor applying the mapping function
   */
  def mappedSingle[A, B](ext: CliExtractor[SingleOptionValue[A]])(f: A => B): CliExtractor[SingleOptionValue[B]] =
    ext.mapWithContext { (triedResult, context) =>
      val mappedResult = triedResult flatMap { optResult =>
        paramTry(context, ext.key)(optResult.map(f))
      }
      (mappedResult, context)
    }

  /**
   * Checks whether all parameters passed via the command line have been
   * consumed. This is a test to find out whether invalid parameters have been
   * specified. During parameter extraction, all parameters that have been
   * accessed are marked. If at the end parameters remain that are not marked,
   * this means that the user has specified unknown or superfluous ones. In
   * this case, parameter validation should fail and no action should be
   * executed by the application. In case, this function detects unused
   * parameters, it returns a ''Failure'' with a
   * [[ParameterExtractionException]]; this exception contains failures for
   * all the unused keys found. Otherwise, result is a ''Success'' with the
   * same ''ParameterContext''.
   *
   * @param paramContext the ''ParameterContext'', updated by all extract
   *                     operations
   * @return a ''Try'' with the validated ''ParameterContext''
   */
  def checkParametersConsumed(paramContext: ParameterContext): Try[ParameterContext] =
    if (paramContext.parameters.allKeysAccessed) Success(paramContext)
    else {
      val failures = paramContext.parameters.notAccessedKeys map { key =>
        ExtractionFailure(key, "Unexpected parameter", paramContext)
      }
      Failure(ParameterExtractionException(failures.toList))
    }

  /**
   * Helper function to create an object representation for a set of
   * components that have been extracted from command line options. The
   * function checks whether all components are successful. If so, the given
   * creator is invoked. Otherwise, result is a failure with an exception
   * that contains all error messages concatenated.
   *
   * @param components the single components
   * @param creator    the function to create the representation
   * @tparam T the type of the representation
   * @return a ''Try'' with the representation or the error messages
   */
  def createRepresentationN[T](components: Try[_]*)(creator: => T): Try[T] = {
    val failures = collectErrorMessages(components: _*)
    if (failures.isEmpty) Success(creator)
    else Failure(ParameterExtractionException(failures))
  }

  /**
   * Creates an object representation from 2 extracted components using a
   * creator function.
   *
   * @param c1      component 1
   * @param c2      component 2
   * @param fCreate the creator function
   * @tparam A type of component 1
   * @tparam B type of component 2
   * @tparam T the type of the object representation
   * @return a ''Try'' with the resulting object
   */
  def createRepresentation[A, B, T](c1: Try[A], c2: Try[B])(fCreate: (A, B) => T): Try[T] =
    createRepresentationN(c1, c2)(fCreate(c1.get, c2.get))

  /**
   * Creates an object representation from 3 extracted components using a
   * creator function.
   *
   * @param c1      component 1
   * @param c2      component 2
   * @param c3      component 3
   * @param fCreate the creator function
   * @tparam A type of component 1
   * @tparam B type of component 2
   * @tparam C type of component 3
   * @tparam T the type of the object representation
   * @return a ''Try'' with the resulting object
   */
  def createRepresentation[A, B, C, T](c1: Try[A], c2: Try[B], c3: Try[C])(fCreate: (A, B, C) => T): Try[T] =
    createRepresentationN(c1, c2, c3)(fCreate(c1.get, c2.get, c3.get))

  /**
   * Creates an object representation from 4 extracted components using a
   * creator function.
   *
   * @param c1      component 1
   * @param c2      component 2
   * @param c3      component 3
   * @param c4      component 4
   * @param fCreate the creator function
   * @tparam A type of component 1
   * @tparam B type of component 2
   * @tparam C type of component 3
   * @tparam D type of component 4
   * @tparam T the type of the object representation
   * @return a ''Try'' with the resulting object
   */
  def createRepresentation[A, B, C, D, T](c1: Try[A], c2: Try[B], c3: Try[C], c4: Try[D])
                                         (fCreate: (A, B, C, D) => T): Try[T] =
    createRepresentationN(c1, c2, c3, c4)(fCreate(c1.get, c2.get, c3.get, c4.get))

  /**
   * Creates an object representation from 5 extracted components using a
   * creator function.
   *
   * @param c1      component 1
   * @param c2      component 2
   * @param c3      component 3
   * @param c4      component 4
   * @param c5      component 5
   * @param fCreate the creator function
   * @tparam A type of component 1
   * @tparam B type of component 2
   * @tparam C type of component 3
   * @tparam D type of component 4
   * @tparam E type of component 5
   * @tparam T the type of the object representation
   * @return a ''Try'' with the resulting object
   */
  def createRepresentation[A, B, C, D, E, T](c1: Try[A], c2: Try[B], c3: Try[C], c4: Try[D], c5: Try[E])
                                            (fCreate: (A, B, C, D, E) => T): Try[T] =
    createRepresentationN(c1, c2, c3, c4, c5)(fCreate(c1.get, c2.get, c3.get, c4.get, c5.get))

  /**
   * Creates an object representation from 6 extracted components using a
   * creator function.
   *
   * @param c1      component 1
   * @param c2      component 2
   * @param c3      component 3
   * @param c4      component 4
   * @param c5      component 5
   * @param c6      component 6
   * @param fCreate the creator function
   * @tparam A type of component 1
   * @tparam B type of component 2
   * @tparam C type of component 3
   * @tparam D type of component 4
   * @tparam E type of component 5
   * @tparam F type of component 6
   * @tparam T the type of the object representation
   * @return a ''Try'' with the resulting object
   */
  def createRepresentation[A, B, C, D, E, F, T](c1: Try[A], c2: Try[B], c3: Try[C], c4: Try[D], c5: Try[E],
                                                c6: Try[F])
                                               (fCreate: (A, B, C, D, E, F) => T): Try[T] =
    createRepresentationN(c1, c2, c3, c4, c5, c6)(fCreate(c1.get, c2.get, c3.get, c4.get, c5.get, c6.get))

  /**
   * Creates an object representation from 7 extracted components using a
   * creator function.
   *
   * @param c1      component 1
   * @param c2      component 2
   * @param c3      component 3
   * @param c4      component 4
   * @param c5      component 5
   * @param c6      component 6
   * @param c7      component 7
   * @param fCreate the creator function
   * @tparam A type of component 1
   * @tparam B type of component 2
   * @tparam C type of component 3
   * @tparam D type of component 4
   * @tparam E type of component 5
   * @tparam F type of component 6
   * @tparam G type of component 7
   * @tparam T the type of the object representation
   * @return a ''Try'' with the resulting object
   */
  def createRepresentation[A, B, C, D, E, F, G, T](c1: Try[A], c2: Try[B], c3: Try[C], c4: Try[D], c5: Try[E],
                                                   c6: Try[F], c7: Try[G])
                                                  (fCreate: (A, B, C, D, E, F, G) => T): Try[T] =
    createRepresentationN(c1, c2, c3, c4, c5, c6, c7)(fCreate(c1.get, c2.get, c3.get, c4.get, c5.get, c6.get,
      c7.get))

  /**
   * Creates an object representation from 8 extracted components using a
   * creator function.
   *
   * @param c1      component 1
   * @param c2      component 2
   * @param c3      component 3
   * @param c4      component 4
   * @param c5      component 5
   * @param c6      component 6
   * @param c7      component 7
   * @param c8      component 8
   * @param fCreate the creator function
   * @tparam A type of component 1
   * @tparam B type of component 2
   * @tparam C type of component 3
   * @tparam D type of component 4
   * @tparam E type of component 5
   * @tparam F type of component 6
   * @tparam G type of component 7
   * @tparam H type of component 8
   * @tparam T the type of the object representation
   * @return a ''Try'' with the resulting object
   */
  def createRepresentation[A, B, C, D, E, F, G, H, T](c1: Try[A], c2: Try[B], c3: Try[C], c4: Try[D], c5: Try[E],
                                                      c6: Try[F], c7: Try[G], c8: Try[H])
                                                     (fCreate: (A, B, C, D, E, F, G, H) => T): Try[T] =
    createRepresentationN(c1, c2, c3, c4, c5, c6, c7, c8)(fCreate(c1.get, c2.get, c3.get, c4.get, c5.get, c6.get,
      c7.get, c8.get))

  /**
   * Creates an object representation from 9 extracted components using a
   * creator function.
   *
   * @param c1      component 1
   * @param c2      component 2
   * @param c3      component 3
   * @param c4      component 4
   * @param c5      component 5
   * @param c6      component 6
   * @param c7      component 7
   * @param c8      component 8
   * @param c9      component 9
   * @param fCreate the creator function
   * @tparam A type of component 1
   * @tparam B type of component 2
   * @tparam C type of component 3
   * @tparam D type of component 4
   * @tparam E type of component 5
   * @tparam F type of component 6
   * @tparam G type of component 7
   * @tparam H type of component 8
   * @tparam I type of component 9
   * @tparam T the type of the object representation
   * @return a ''Try'' with the resulting object
   */
  def createRepresentation[A, B, C, D, E, F, G, H, I, T](c1: Try[A], c2: Try[B], c3: Try[C], c4: Try[D], c5: Try[E],
                                                         c6: Try[F], c7: Try[G], c8: Try[H], c9: Try[I])
                                                        (fCreate: (A, B, C, D, E, F, G, H, I) => T): Try[T] =
    createRepresentationN(c1, c2, c3, c4, c5, c6, c7, c8, c9)(fCreate(c1.get, c2.get, c3.get, c4.get, c5.get, c6.get,
      c7.get, c8.get, c9.get))

  /**
   * Creates an object representation from 10 extracted components using a
   * creator function.
   *
   * @param c1      component 1
   * @param c2      component 2
   * @param c3      component 3
   * @param c4      component 4
   * @param c5      component 5
   * @param c6      component 6
   * @param c7      component 7
   * @param c8      component 8
   * @param c9      component 9
   * @param c10     component 10
   * @param fCreate the creator function
   * @tparam A type of component 1
   * @tparam B type of component 2
   * @tparam C type of component 3
   * @tparam D type of component 4
   * @tparam E type of component 5
   * @tparam F type of component 6
   * @tparam G type of component 7
   * @tparam H type of component 8
   * @tparam I type of component 9
   * @tparam J type of component 10
   * @tparam T the type of the object representation
   * @return a ''Try'' with the resulting object
   */
  def createRepresentation[A, B, C, D, E, F, G, H, I, J, T](c1: Try[A], c2: Try[B], c3: Try[C], c4: Try[D],
                                                            c5: Try[E], c6: Try[F], c7: Try[G], c8: Try[H],
                                                            c9: Try[I], c10: Try[J])
                                                           (fCreate: (A, B, C, D, E, F, G, H, I, J) => T): Try[T] =
    createRepresentationN(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10)(fCreate(c1.get, c2.get, c3.get, c4.get, c5.get,
      c6.get, c7.get, c8.get, c9.get, c10.get))

  /**
   * Creates an object representation from 11 extracted components using a
   * creator function.
   *
   * @param c1      component 1
   * @param c2      component 2
   * @param c3      component 3
   * @param c4      component 4
   * @param c5      component 5
   * @param c6      component 6
   * @param c7      component 7
   * @param c8      component 8
   * @param c9      component 9
   * @param c10     component 10
   * @param c11     component 11
   * @param fCreate the creator function
   * @tparam A type of component 1
   * @tparam B type of component 2
   * @tparam C type of component 3
   * @tparam D type of component 4
   * @tparam E type of component 5
   * @tparam F type of component 6
   * @tparam G type of component 7
   * @tparam H type of component 8
   * @tparam I type of component 9
   * @tparam J type of component 10
   * @tparam K type of component 11
   * @tparam T the type of the object representation
   * @return a ''Try'' with the resulting object
   */
  def createRepresentation[A, B, C, D, E, F, G, H, I, J, K, T](c1: Try[A], c2: Try[B], c3: Try[C], c4: Try[D],
                                                               c5: Try[E], c6: Try[F], c7: Try[G], c8: Try[H],
                                                               c9: Try[I], c10: Try[J], c11: Try[K])
                                                              (fCreate: (A, B, C, D, E, F, G, H, I, J, K) => T):
  Try[T] =
    createRepresentationN(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11)(fCreate(c1.get, c2.get, c3.get, c4.get, c5.get,
      c6.get, c7.get, c8.get, c9.get, c10.get, c11.get))

  /**
   * Creates an object representation from 12 extracted components using a
   * creator function.
   *
   * @param c1      component 1
   * @param c2      component 2
   * @param c3      component 3
   * @param c4      component 4
   * @param c5      component 5
   * @param c6      component 6
   * @param c7      component 7
   * @param c8      component 8
   * @param c9      component 9
   * @param c10     component 10
   * @param c11     component 11
   * @param c12     component 12
   * @param fCreate the creator function
   * @tparam A type of component 1
   * @tparam B type of component 2
   * @tparam C type of component 3
   * @tparam D type of component 4
   * @tparam E type of component 5
   * @tparam F type of component 6
   * @tparam G type of component 7
   * @tparam H type of component 8
   * @tparam I type of component 9
   * @tparam J type of component 10
   * @tparam K type of component 11
   * @tparam L type of component 12
   * @tparam T the type of the object representation
   * @return a ''Try'' with the resulting object
   */
  def createRepresentation[A, B, C, D, E, F, G, H, I, J, K, L, T](c1: Try[A], c2: Try[B], c3: Try[C], c4: Try[D],
                                                                  c5: Try[E], c6: Try[F], c7: Try[G], c8: Try[H],
                                                                  c9: Try[I], c10: Try[J], c11: Try[K], c12: Try[L])
                                                                 (fCreate: (A, B, C, D, E, F, G, H, I, J, K, L) => T):
  Try[T] =
    createRepresentationN(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12)(fCreate(c1.get, c2.get, c3.get, c4.get,
      c5.get, c6.get, c7.get, c8.get, c9.get, c10.get, c11.get, c12.get))

  /**
   * Creates an object representation from 13 extracted components using a
   * creator function.
   *
   * @param c1      component 1
   * @param c2      component 2
   * @param c3      component 3
   * @param c4      component 4
   * @param c5      component 5
   * @param c6      component 6
   * @param c7      component 7
   * @param c8      component 8
   * @param c9      component 9
   * @param c10     component 10
   * @param c11     component 11
   * @param c12     component 12
   * @param c13     component 13
   * @param fCreate the creator function
   * @tparam A type of component 1
   * @tparam B type of component 2
   * @tparam C type of component 3
   * @tparam D type of component 4
   * @tparam E type of component 5
   * @tparam F type of component 6
   * @tparam G type of component 7
   * @tparam H type of component 8
   * @tparam I type of component 9
   * @tparam J type of component 10
   * @tparam K type of component 11
   * @tparam L type of component 12
   * @tparam M type of component 13
   * @tparam T the type of the object representation
   * @return a ''Try'' with the resulting object
   */
  def createRepresentation[A, B, C, D, E, F, G, H, I, J, K, L, M, T](c1: Try[A], c2: Try[B], c3: Try[C], c4: Try[D],
                                                                     c5: Try[E], c6: Try[F], c7: Try[G], c8: Try[H],
                                                                     c9: Try[I], c10: Try[J], c11: Try[K], c12: Try[L],
                                                                     c13: Try[M])
                                                                    (fCreate: (A, B, C, D, E, F, G, H, I, J, K, L,
                                                                      M) => T):
  Try[T] =
    createRepresentationN(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13)(fCreate(c1.get, c2.get, c3.get,
      c4.get, c5.get, c6.get, c7.get, c8.get, c9.get, c10.get, c11.get, c12.get, c13.get))

  /**
   * Creates an object representation from 14 extracted components using a
   * creator function.
   *
   * @param c1      component 1
   * @param c2      component 2
   * @param c3      component 3
   * @param c4      component 4
   * @param c5      component 5
   * @param c6      component 6
   * @param c7      component 7
   * @param c8      component 8
   * @param c9      component 9
   * @param c10     component 10
   * @param c11     component 11
   * @param c12     component 12
   * @param c13     component 13
   * @param c14     component 14
   * @param fCreate the creator function
   * @tparam A type of component 1
   * @tparam B type of component 2
   * @tparam C type of component 3
   * @tparam D type of component 4
   * @tparam E type of component 5
   * @tparam F type of component 6
   * @tparam G type of component 7
   * @tparam H type of component 8
   * @tparam I type of component 9
   * @tparam J type of component 10
   * @tparam K type of component 11
   * @tparam L type of component 12
   * @tparam M type of component 13
   * @tparam N type of component 14
   * @tparam T the type of the object representation
   * @return a ''Try'' with the resulting object
   */
  def createRepresentation[A, B, C, D, E, F, G, H, I, J, K, L, M, N, T](c1: Try[A], c2: Try[B], c3: Try[C], c4: Try[D],
                                                                        c5: Try[E], c6: Try[F], c7: Try[G], c8: Try[H],
                                                                        c9: Try[I], c10: Try[J], c11: Try[K],
                                                                        c12: Try[L], c13: Try[M], c14: Try[N])
                                                                       (fCreate: (A, B, C, D, E, F, G, H, I, J, K, L,
                                                                         M, N) => T):
  Try[T] =
    createRepresentationN(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14)(fCreate(c1.get, c2.get,
      c3.get, c4.get, c5.get, c6.get, c7.get, c8.get, c9.get, c10.get, c11.get, c12.get, c13.get, c14.get))

  /**
   * Creates an object representation from 15 extracted components using a
   * creator function.
   *
   * @param c1      component 1
   * @param c2      component 2
   * @param c3      component 3
   * @param c4      component 4
   * @param c5      component 5
   * @param c6      component 6
   * @param c7      component 7
   * @param c8      component 8
   * @param c9      component 9
   * @param c10     component 10
   * @param c11     component 11
   * @param c12     component 12
   * @param c13     component 13
   * @param c14     component 14
   * @param c15     component 15
   * @param fCreate the creator function
   * @tparam A type of component 1
   * @tparam B type of component 2
   * @tparam C type of component 3
   * @tparam D type of component 4
   * @tparam E type of component 5
   * @tparam F type of component 6
   * @tparam G type of component 7
   * @tparam H type of component 8
   * @tparam I type of component 9
   * @tparam J type of component 10
   * @tparam K type of component 11
   * @tparam L type of component 12
   * @tparam M type of component 13
   * @tparam N type of component 14
   * @tparam O type of component 15
   * @tparam T the type of the object representation
   * @return a ''Try'' with the resulting object
   */
  def createRepresentation[A, B, C, D, E, F, G, H,
    I, J, K, L, M, N, O, T](c1: Try[A], c2: Try[B], c3: Try[C], c4: Try[D], c5: Try[E], c6: Try[F], c7: Try[G],
                            c8: Try[H], c9: Try[I], c10: Try[J], c11: Try[K], c12: Try[L], c13: Try[M], c14: Try[N],
                            c15: Try[O])(fCreate: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => T):
  Try[T] =
    createRepresentationN(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15)(fCreate(c1.get,
      c2.get, c3.get, c4.get, c5.get, c6.get, c7.get, c8.get, c9.get, c10.get, c11.get, c12.get, c13.get, c14.get,
      c15.get))

  /**
   * Executes the given ''CliExtractor'' on the parameters specified and
   * returns its result and the updated ''ParameterContext'' object. Notice
   * that, thanks to an implicit conversion function, it is sufficient to
   * only parse in a ''ParametersMap'' object.
   *
   * @param extractor     the extractor to be executed
   * @param parameters    the current ''Parameters''
   * @param consoleReader the object to read from the console
   * @tparam T the result type of the ''CliExtractor''
   * @return a tuple with the result and the resulting ''ParameterContext''
   */
  def runExtractor[T](extractor: CliExtractor[T], parameters: Parameters)
                     (implicit consoleReader: ConsoleReader): (T, ParameterContext) = {
    val context = ParameterContext(parameters, EmptyModelContext, consoleReader)
    val (result, nextContext) = extractor.run(context)
    (result, nextContext)
  }

  /**
   * Executes the given ''CliExtractor'' that may fail on the parameters
   * specified. Result is a ''Try'' with the extractor's result and the
   * updated ''ParameterContext'' object. This function is useful if a failed
   * extractor should cause the whole operation to fail.
   *
   * @param extractor     the extractor to be executed
   * @param parameters    the current ''Parameters'' object
   * @param consoleReader the object to read from the console
   * @tparam T the result type of the ''CliExtractor''
   * @return a ''Try'' of a tuple with the result and the updated
   *         ''ParameterContext''
   */
  def tryExtractor[T](extractor: CliExtractor[Try[T]], parameters: Parameters)
                     (implicit consoleReader: ConsoleReader): Try[(T, ParameterContext)] = {
    val (triedRes, next) = runExtractor(extractor, parameters)
    triedRes map ((_, next))
  }

  /**
   * Generates a ''Try'' for the given expression that contains a meaningful
   * exception in case of a failure. This function maps the original
   * exception to an ''IllegalArgumentException'' with a message that contains
   * the name of the parameter.
   *
   * @param context the ''ParameterContext''
   * @param key     the parameter key
   * @param f       the expression
   * @tparam T the result type of the expression
   * @return a succeeded ''Try'' with the expression value or a failed ''Try''
   *         with a meaningful exception
   */
  def paramTry[T](context: ParameterContext, key: String)(f: => T): Try[T] =
    Try(f) recoverWith {
      case pex: ParameterExtractionException => Failure(pex)
      case ex => Failure(paramException(context, key, ex.getMessage, ex))
    }

  /**
   * Generates an exception that reports a problem with a specific command
   * line option. These exceptions have a special type.
   *
   * @param context the ''ParameterContext''
   * @param key     the option key
   * @param message the error message
   * @param cause   an option cause of the error
   * @return the resulting exception
   */
  def paramException(context: ParameterContext, key: String, message: String, cause: Throwable = null):
  ParameterExtractionException = {
    val failure = ExtractionFailure(key, generateErrorMessage(message, cause), context)
    ParameterExtractionException(failure)
  }

  /**
   * Updates the given model context to contain all the error messages from the
   * failures provided. For each ''ExtractionFailure'', the error message is
   * added to the corresponding attribute of the option affected. The modified
   * model context can then be used to generate formatted output with all error
   * messages.
   *
   * @param modelContext the model context to be used as base
   * @param failures     a collection with failures during extraction
   * @return the modified model context
   */
  def addFailuresToModelContext(modelContext: ModelContext, failures: Iterable[ExtractionFailure]): ModelContext =
    failures.foldLeft(modelContext) { (ctx, failure) =>
      ctx.addOption(failure.key, None)
        .addAttribute(ParameterModel.AttrErrorMessage, failure.message)
    }

  /**
   * Runs the given ''CliExtractor'' against a dummy parameter context to
   * obtain metadata from it. This run will populate a ''ModelContext''
   * with information about all the options accessed by the extractor.
   *
   * @param extractor    the ''CliExtractor'' in question
   * @param parameters   the parameters to store in the context
   * @param modelContext the initial model context for the context
   * @return the ''ParameterContext'' with updated metadata
   */
  def gatherMetaData(extractor: CliExtractor[_], parameters: ParametersMap = Map.empty,
                     modelContext: ModelContext = EmptyModelContext): ParameterContext = {
    val paramCtx = contextForMetaDataRun(parameters, modelContext)
    extractor.run(paramCtx)._2
  }

  /**
   * Returns a collection containing all extraction failures from the given
   * components. This is used to create an object representation of a group of
   * command line arguments. Only if all components could be extracted
   * successfully, the representation can be created. Otherwise, a list with
   * all errors is returned. The resulting collection is also an indicator
   * whether the representation can be created: if it is empty, there are no
   * errors.
   *
   * @param components the single components
   * @return a collection with ''ExtractionFailure'' extracted from the
   *         components
   */
  private def collectErrorMessages(components: Try[_]*): List[ExtractionFailure] =
    components.foldRight(List.empty[ExtractionFailure]) { (c, list) =>
      c match {
        case Failure(exception: ParameterExtractionException) =>
          exception.failures ::: list
        case Failure(exception) =>
          failureFor(exception) :: list
        case _ => list
      }
    }

  /**
   * Generates an ''ExtractionFailure'' object from an arbitrary exception.
   * As the exception does not contain specific failure information, some
   * fields are initialized with dummy values.
   *
   * @param exception the exception
   * @return the resulting ''ExtractionFailure''
   */
  private def failureFor(exception: Throwable): ExtractionFailure =
    ExtractionFailure(message = exception.getMessage, key = "", context = DummyParameterContext)

  /**
   * Updates a model context by running some extractors against it. This
   * function is typically used by conditional extractors that select some
   * extractors to be executed from a larger set of extractors. The other
   * extractors that are not selected still need to be reflected by the model
   * context. This function expects a list of extractors and the optional
   * groups they belong to. It runs them against a dummy parameter context, so
   * that the model context is updated, but the original parameter context is
   * not modified.
   *
   * @param modelContext        the current ''ModelContext''
   * @param extractorsAndGroups a list with extractors and their groups
   * @tparam A the result type of the extractors
   * @return the updated model context
   */
  private def updateHelpContext[A](modelContext: ModelContext,
                                   extractorsAndGroups: List[(CliExtractor[A], Option[String])]): ModelContext =
    extractorsAndGroups
      .foldLeft(modelContext) { (helpCtx, p) =>
        val helpCtxWithGroup = helpCtx startGroupConditionally p._2
        val nextContext = gatherMetaData(p._1, modelContext = helpCtxWithGroup)
        nextContext.helpContext.endGroupConditionally(p._2)
      }

  /**
   * Generates the error message for an exception encountered during parameter
   * processing. If there is a cause available, the exception class name is
   * added to the message.
   *
   * @param message the original error message
   * @param cause   the causing exception
   * @return the enhanced error message
   */
  private def generateErrorMessage(message: String, cause: Throwable): String = {
    val exceptionName = if (cause == null) ""
    else cause.getClass.getName + " - "
    s"$exceptionName$message"
  }

  /**
   * Generates a description of a constant option value based on the concrete
   * value(s).
   *
   * @param values the constant values of this option
   * @tparam A the type of the values
   * @return the resulting value description
   */
  private def generateValueDescription[A](values: List[A]): String =
    values match {
      case h :: Nil => h.toString
      case l => l.mkString("<", ", ", ">")
    }

  /**
   * Returns a ''ParameterContext'' to be used for the invocation of a
   * ''CliExtractor'' if only meta data of the extractor is of interest. This
   * context has no parameter values and dummy helper objects. Only the help
   * context is set and will be updated during the run.
   *
   * @param params      the parameters for the context
   * @param helpContext the ''ModelContext''
   * @return the ''ParameterContext'' for the meta data run
   */
  private def contextForMetaDataRun(params: ParametersMap, helpContext: ModelContext): ParameterContext =
    ParameterContext(Parameters(params, Set.empty), helpContext, DummyConsoleReader)
}
