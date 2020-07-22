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

import com.github.scli.ParameterModel._

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/**
 * A module providing functionality related to the generation of help
 * information for a command line interface.
 *
 * When defining the options and input parameters supported by a CLI help
 * texts can be specified; some other meta information is collected
 * automatically. This module defines classes to collect this information and
 * to generate help or usage texts out of it.
 */
object CliHelpGenerator {

  /** The prefix indicating a command line option. */
  final val DefaultOptionPrefix = "--"

  /**
   * A standard sort function for parameters that implements an alphabetic
   * ordering (which is case-insensitive).
   */
  final val AlphabeticParameterSortFunc: ParameterSortFunc =
    _.sortWith((d1, d2) => toUpperCase(d1.key) < toUpperCase(d2.key))

  /** A standard filter function which accepts all parameters. */
  final val AllFilterFunc: ParameterFilter = _ => true

  /**
   * A standard filter function that accepts only elements with the parameter
   * type ''option''.
   */
  final val OptionsFilterFunc: ParameterFilter = parameterTypeFilter(ParameterTypeOption)

  /**
   * A standard filter function that accepts only elements with the parameter
   * type ''input''. This function can be used to deal only with input
   * parameters.
   */
  final val InputParamsFilterFunc: ParameterFilter = parameterTypeFilter(ParameterTypeInput)

  /**
   * A standard filter function that accepts only elements that are not
   * assigned to any group. These are usually top-level options that do not
   * depend on a context, but are always valid.
   */
  final val UnassignedGroupFilterFunc: ParameterFilter =
    data => !data.attributes.attributes.contains(AttrGroup)

  /** The default padding string to separate columns of the help text. */
  final val DefaultPadding: String = "  "

  /**
   * The platform-specific line separator. This is used as line feed character
   * between two lines of the help text.
   */
  final val CR = System.lineSeparator()

  /**
   * Default formatting symbols used to generate the overview of input
   * parameters.
   */
  final val DefaultInputParamSymbols = InputParamOverviewSymbols(keyPrefix = "<", keySuffix = ">",
    optionalPrefix = "[", optionalSuffix = "]", ellipsis = "...")

  /**
   * A case class defining symbols to be used when generating the overview of
   * input parameters.
   *
   * The strings defined by this class are used to decorate the keys of input
   * parameters to indicate specific properties.
   *
   * @param keyPrefix      the prefix added before parameter keys
   * @param keySuffix      the suffix added after parameter keys
   * @param optionalPrefix prefix to mark an element as optional
   * @param optionalSuffix suffix to mark an element as optional
   * @param ellipsis       string to represent a gap between parameter indices
   */
  case class InputParamOverviewSymbols(keyPrefix: String,
                                       keySuffix: String,
                                       optionalPrefix: String,
                                       optionalSuffix: String,
                                       ellipsis: String) {
    /**
     * Decorates a key with the correct symbols.
     *
     * @param key the key
     * @return the decorated key
     */
    def decorateKey(key: String): String = s"$keyPrefix$key$keySuffix"

    /**
     * Conditionally adds markers to the given string to indicate that it
     * represents an optional value.
     *
     * @param str      the string
     * @param optional the flag whether the value is optional
     * @return the resulting string
     */
    def markAsOptional(str: String, optional: Boolean): String =
      if (optional) optionalPrefix + str + optionalSuffix else str
  }

  /**
   * Checks whether the option whose attributes are provided belongs to the
   * given group.
   *
   * @param attrs the ''OptionAttributes''
   * @param group the name of the group
   * @return a flag whether this option belongs to this group
   */
  def isInGroup(attrs: ParameterAttributes, group: String): Boolean =
    attrs.attributes.get(AttrGroup) exists (_ contains group + GroupSeparator)

  /**
   * Returns a set with the names of all groups the option whose attributes
   * are provided belongs to.
   *
   * @param attrs the ''OptionAttributes''
   * @return a set with the names of all groups
   */
  def groups(attrs: ParameterAttributes): Set[String] =
    attrs.attributes.get(AttrGroup).map(_.split(GroupSeparator).toSet) getOrElse Set.empty

  /**
   * Type definition of a function that sorts the list of parameters in the
   * generated help text.
   */
  type ParameterSortFunc = Seq[ParameterMetaData] => Seq[ParameterMetaData]

  /**
   * Type definition for a predicate to filter parameters from a
   * [[ModelContext]].
   */
  type ParameterFilter = ParameterMetaData => Boolean

  /**
   * Type definition of a function that generates a column of a help text of
   * an option. The column can consist of multiple lines of text hence, the
   * result is a list of strings). For each option, multiple columns can be
   * generated that are defined by specifying the corresponding generator
   * functions.
   */
  type ColumnGenerator = ParameterMetaData => List[String]

  /**
   * Generates a tabular help text for the command line options of an
   * application. For each option, a number of columns is displayed that are
   * defined by a sequence of ''ColumnGenerator'' functions. The table is
   * converted to a string that can be directly printed to the console. By
   * passing in additional parameters, the output can be customized.
   *
   * @param context    the ''ModelContext'' with all meta data about options
   * @param sortFunc   a function to sort the list of options; per default,
   *                   options are sorted alphabetically ignoring case
   * @param filterFunc a function to filter the options to be displayed; per
   *                   default, all options are shown
   * @param padding    a padding string inserted between columns
   * @param optNewline an optional string added between the help texts of
   *                   different options; by changing this string, one could
   *                   add for instance empty lines or horizontal bars
   * @param columns    the functions to generate the single columns
   * @return a string with the help for command line options
   */
  def generateOptionsHelp(context: ModelContext,
                          sortFunc: ParameterSortFunc = AlphabeticParameterSortFunc,
                          filterFunc: ParameterFilter = AllFilterFunc,
                          padding: String = DefaultPadding,
                          optNewline: Option[String] = Some(""))
                         (columns: ColumnGenerator*): String = {

    // generates the columns of an option by applying the column generators
    def generateColumns(data: ParameterMetaData): Seq[List[String]] =
      columns.map(_.apply(data))

    val metaData = context.optionMetaData
      .filter(filterFunc)
      .toSeq
    if (metaData.isEmpty) ""
    else {
      val rows = sortFunc(metaData)
        .map(generateColumns)
      val widths = rows map columnWidths
      val maxWidths = widths.transpose.map(_.max)
      val spaces = paddingString(maxWidths)

      // generates the row for an option that can consist of multiple lines;
      // the lines have to be correctly aligned and padded
      def generateRow(columns: Seq[List[String]]): Seq[String] = {
        val maxLineCount = columns.map(_.size).max
        val emptyList = List.fill(maxLineCount)("")
        val filledColumns = columns.map(list => growList(list, maxLineCount, emptyList))

        val lines = filledColumns.transpose.map(_.zip(maxWidths))
          .map { line =>
            line.map { t =>
              val cell = t._1
              cell + spaces.substring(0, t._2 - cell.length)
            }.mkString(padding)
          }
        optNewline.fold(lines)(lines.toList :+ _)
      }

      val generatedRows = rows.flatMap(generateRow)
      optNewline.fold(generatedRows)(_ => generatedRows.dropRight(1)) // there is 1 newline too much
        .mkString(CR)
    }
  }

  /**
   * A special ''OptionSortFunc'' that handles input parameters. The
   * parameters are sorted based on their expected order in the command line.
   * The function expects the list of options to be ordered has been filtered
   * to contain input parameters only.
   *
   * @param helpContext the ''ModelContext''
   * @return the function to sort input parameter options
   */
  def inputParamSortFunc(helpContext: ModelContext): ParameterSortFunc = {
    def paramIndex(key: String): Int =
      helpContext.inputs.find(_.key == key).map(_.index) getOrElse helpContext.inputs.size

    options =>
      options.map(data => (data, paramIndex(data.key)))
        .sortWith(_._2 < _._2)
        .map(_._1)
  }

  /**
   * Returns a filter function that accepts only options belonging to all of
   * the given groups.
   *
   * @param groups the name of the groups the options must belong to
   * @return the function that filters for all of these groups
   */
  def groupFilterFunc(groups: String*): ParameterFilter =
    data =>
      groups.forall(group => isInGroup(data.attributes, group))

  /**
   * Returns a filter function that accepts only options that have the given
   * attribute.
   *
   * @param attrKey the key of the required attribute
   * @return the function that filters for options with this attribute
   */
  def attributeFilterFunc(attrKey: String): ParameterFilter =
    data => data.attributes.attributes contains attrKey

  /**
   * Returns a filter function implementing AND logic. The resulting filter
   * accepts an element if and only if all of the filters provided accept it.
   *
   * @param filters the filters to be combined
   * @return a combined filter function with AND semantics
   */
  def andFilter(filters: ParameterFilter*): ParameterFilter =
    data => filters.forall(f => f(data))

  /**
   * Returns a filter function implementing OR logic. The resulting filter
   * accepts an element as soon as one of the filters provided accepts it.
   *
   * @param filters the filters to be combined
   * @return a combined filter function with OR semantics
   */
  def orFilter(filters: ParameterFilter*): ParameterFilter =
    data => filters.exists(f => f(data))

  /**
   * Returns a filter that yields the opposite result of the filter provided.
   * This filter can be used for instance when exclusion logic is needed.
   *
   * @param filter the original filter
   * @return the negated filter
   */
  def negate(filter: ParameterFilter): ParameterFilter =
    data => !filter(data)

  /**
   * Returns a ''ColumnGenerator'' function that produces a single text line
   * from the value of the attribute specified. If the attribute is not
   * present, result is an empty list.
   *
   * @param attrKey the key of the attribute to be read
   * @return the ''ColumnGenerator'' reading this attribute
   */
  def attributeColumnGenerator(attrKey: String): ColumnGenerator = data =>
    data.attributes.attributes.get(attrKey) map (List(_)) getOrElse List.empty

  /**
   * Returns a ''ColumnGenerator'' function that generates the name of the
   * current command line parameter. This can be used for instance in the first
   * column to display the key the following information is about.
   *
   * @param optionPrefix a prefix added to the parameter name
   * @return the ''ColumnGenerator'' generating the parameter name
   */
  def parameterNameColumnGenerator(optionPrefix: String = DefaultOptionPrefix): ColumnGenerator =
    data => List(optionPrefix + data.key)

  /**
   * Returns a ''ColumnGenerator'' that renders the multiplicity attribute.
   * This is a convenience function that correctly shows the default
   * multiplicity if none is provided.
   *
   * @return the ''ColumnGenerator'' producing the options' multiplicity
   */
  def multiplicityColumnGenerator: ColumnGenerator =
    defaultValueColumnGenerator(attributeColumnGenerator(AttrMultiplicity), Multiplicity.UnspecifiedMultiplicityString)

  /**
   * Returns a ''ColumnGenerator'' function that applies a default value to
   * another generator function. The passed in function is invoked first. If
   * it does not yield any values, the default values are returned.
   *
   * @param generator     the generator function to decorate
   * @param defaultValues the default values
   * @return the ''ColumnGenerator'' applying default values
   */
  def defaultValueColumnGenerator(generator: ColumnGenerator, defaultValues: String*): ColumnGenerator = {
    val defaultList = defaultValues.toList
    data =>
      generator(data) match {
        case l@_ :: _ => l
        case _ => defaultList
      }
  }

  /**
   * Returns a ''ColumnGenerator'' function that allows adding prefix lines to
   * or a prefix text to all lines of another generator function. This is
   * useful for instance to add a caption to an attribute value. The prefix is
   * added only if the decorated generator function yields a value; otherwise,
   * this generator returns an empty list, too.
   *
   * @param generator   the generator function to decorate
   * @param prefixLines a list with lines to add as prefix to the generator
   * @param prefixText  a text to add as prefix to each original line
   * @return the ''CliHelpGenerator'' generating a prefix
   */
  def prefixColumnGenerator(generator: ColumnGenerator, prefixLines: List[String] = Nil,
                            prefixText: Option[String] = None): ColumnGenerator =
    data =>
      generator(data) match {
        case l@_ :: _ =>
          val lines = prefixText.fold(generator(data))(pref => l.map(pref + _))
          prefixLines ++ lines
        case l => l
      }

  /**
   * Returns a ''ColumnGenerator'' function that wraps the text lines returned
   * by another generator. The text lines returned by the decorated generator
   * are split at newline characters (unless there is no continuation
   * character '\' before the newline). The resulting lines are wrapped at the
   * given line length.
   *
   * @param generator  the generator function to decorate
   * @param lineLength the line length for wrapping
   * @return the ''ColumnGenerator'' doing wrapping
   */
  def wrapColumnGenerator(generator: ColumnGenerator, lineLength: Int): ColumnGenerator =
    data =>
      generator(data)
        .map(handleLineContinuations)
        .flatMap(splitLines)
        .flatMap(wrapLine(_, lineLength))

  /**
   * Returns a ''ColumnGenerator'' function that composes the results of all
   * the passed in generators. Using this function, the cells of the tabular
   * option help can contain the data of multiple generators.
   *
   * @param generators the sequence of generator functions to combine
   * @return the ''ColumnGenerator'' that composes these generators
   */
  def composeColumnGenerator(generators: ColumnGenerator*): ColumnGenerator =
    data => generators.flatMap(g => g(data)).toList

  /**
   * Generates an overview over the input parameters of an application. The
   * function generates a list where each element represents an input
   * parameter. The information about a single parameter is a visual
   * representation of the key and its multiplicity. Having this information
   * available as a list of strings allows the caller to integrate it into
   * other output; e.g. by doing a ''mkstring(" ")'', it can be displayed on a
   * single line.
   *
   * @param modelContext the model context
   * @param symbols      defines the symbols to indicate certain parameter
   *                     properties
   * @return a list with the information about all input parameters
   */
  def generateInputParamsOverview(modelContext: ModelContext,
                                  symbols: InputParamOverviewSymbols = DefaultInputParamSymbols): List[String] =
    modelContext.inputs.toList
      .map(inputParameterOverview(modelContext, _, symbols))

  /**
   * Calculates the width of all the columns of a single row in a table of
   * option data.
   *
   * @param row the row (consisting of multiple columns)
   * @return a sequence with the widths of the columns
   */
  private def columnWidths(row: Seq[List[String]]): Seq[Int] =
    row map cellWidth

  /**
   * Calculates the maximum width of a cell in a table of option data. The
   * cell can consist of multiple lines. The maximum line length is returned.
   *
   * @param data the data in the cell
   * @return the length of this cell
   */
  private def cellWidth(data: List[String]): Int =
    if (data.nonEmpty) data.map(_.length).max
    else 0

  /**
   * Makes sure that a list has the given size by appending elements of a list
   * with empty elements.
   *
   * @param list      the list to be manipulated
   * @param toSize    the desired target size
   * @param emptyList a list containing empty elements
   * @return the list with the target size
   */
  private def growList(list: List[String], toSize: Int, emptyList: List[String]): List[String] =
    if (list.size >= toSize) list
    else list ++ emptyList.slice(0, toSize - list.size)

  /**
   * Generates a string with a number of spaces that is used to pad the cells
   * of a table to a certain length. The length of the string is determined
   * from the maximum column width.
   *
   * @param colWidths the column widths
   * @return the padding string
   */
  private def paddingString(colWidths: Seq[Int]): String = {
    val maxSpaces = colWidths.max
    " " * maxSpaces
  }

  /**
   * Removes newline characters that follow a line continuation character.
   * This makes it easier to define longer help texts in source code using
   * multi-line strings. Each line break in such a string adds a newline
   * character, which might not be desired.
   *
   * @param s the string to be processed
   * @return the string with superfluous newlines removed
   */
  private def handleLineContinuations(s: String): String =
    s.replace("\\\n", "")

  /**
   * Splits a string at newline characters and returns a list with the single
   * lines.
   *
   * @param s the string to be split
   * @return a list with the lines in the string
   */
  private def splitLines(s: String): List[String] =
    s.split("\n").toList

  /**
   * Determines the position where to wrap a string to adhere to the given
   * line length and the start position of the next line. This function tries
   * to find a space character at or before the line length. The space itself
   * is skipped. If no space character can be found in the line, the wrap
   * position is set to the maximum length. This function expects that the
   * input string is longer than the line length.
   *
   * @param s      the string
   * @param length the maximum line length
   * @return the positions where to break the string and to start the next
   *         line
   */
  private def wrapPositions(s: String, length: Int): (Int, Int) = {
    val posSpace = s.lastIndexOf(" ", length)
    if (posSpace <= 0) (length, length) else (posSpace, posSpace + 1)
  }

  /**
   * Wraps the given string, so that lines are no longer than the line length
   * specified. The lines are broken at space characters if possible.
   *
   * @param s      the string to be wrapped
   * @param length the maximum line length
   * @return the single wrapped lines of the string
   */
  private def wrapLine(s: String, length: Int): List[String] = {
    @tailrec def doWrap(line: String, builder: ListBuffer[String]): List[String] =
      if (line.length < length) {
        builder += line
        builder.toList
      } else {
        val (wrapPos, nextStart) = wrapPositions(line, length)
        builder += line.substring(0, wrapPos)
        doWrap(line.substring(nextStart), builder)
      }

    doWrap(s, ListBuffer.empty)
  }

  /**
   * Returns a filter function that filters for parameters of the given type.
   *
   * @param wantedType the parameter type to filter for
   * @return the filter function for this type
   */
  private def parameterTypeFilter(wantedType: String): ParameterFilter =
    data => data.attributes.attributes.get(AttrParameterType) contains wantedType

  /**
   * Generates the part of the overview of an input parameter that is
   * determined by the lower bound of the multiplicity. These values are all
   * mandatory. Depending on the lower bound either a single key or multiple
   * keys need to be displayed.
   *
   * @param key          the key of the input parameter
   * @param multiplicity the multiplicity
   * @param symbols      the formatting symbols
   * @return the lower part of the overview
   */
  private def inputParameterOverviewLowerPart(key: String, multiplicity: Multiplicity,
                                              symbols: InputParamOverviewSymbols): String = {
    if (multiplicity.lower == 2) s"${symbols.decorateKey(key + "1")} ${symbols.decorateKey(key + "2")}"
    else if (multiplicity.lower > 1) s"${symbols.decorateKey(key + "1")}${symbols.ellipsis}" +
      symbols.decorateKey(key + multiplicity.lower)
    else {
      val index = if (!multiplicity.unbounded && multiplicity.upper > multiplicity.lower) "1" else ""
      symbols.decorateKey(key + index)
    }
  }

  /**
   * Generates the part of the overview of an input parameter that is
   * determined by the upper bound of the multiplicity. The upper part is
   * defined only if the upper bound is greater than the lower bound; these
   * values are optional.
   *
   * @param key          the key of the input parameter
   * @param multiplicity the multiplicity
   * @param symbols      the formatting symbols
   * @return the upper part of the overview
   */
  private def inputParameterOverviewUpperPart(key: String, multiplicity: Multiplicity,
                                              symbols: InputParamOverviewSymbols): String =
    if (!multiplicity.unbounded && multiplicity.upper <= multiplicity.lower) ""
    else {
      val result = if (multiplicity.unbounded) symbols.ellipsis
      else {
        val upperKey = s"${symbols.decorateKey(key + multiplicity.upper)}"
        if (multiplicity.upper > multiplicity.lower + 1) symbols.ellipsis + upperKey
        else upperKey
      }
      symbols.markAsOptional(result, multiplicity.mandatory)
    }

  /**
   * Generates the overview of an input parameter based on its key and
   * multiplicity.
   *
   * @param modelContext the model context
   * @param parameterRef the reference to the parameter in question
   * @param symbols      the formatting symbols
   * @return a string with information about this input parameter
   */
  private def inputParameterOverview(modelContext: ModelContext, parameterRef: InputParameterRef,
                                     symbols: InputParamOverviewSymbols): String = {
    val key = parameterRef.key
    val multiplicity = modelContext.options(key).attributes.get(AttrMultiplicity)
      .map(Multiplicity.parse) getOrElse Multiplicity.Unbounded
    val lowerPart = inputParameterOverviewLowerPart(key, multiplicity, symbols)
    val upperPart = inputParameterOverviewUpperPart(key, multiplicity, symbols)
    val result = if (upperPart.nonEmpty) lowerPart + " " + upperPart else lowerPart
    symbols.markAsOptional(result, multiplicity.optional)
  }
}
