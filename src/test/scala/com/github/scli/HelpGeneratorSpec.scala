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

import com.github.scli.HelpGenerator._
import com.github.scli.HelpGeneratorTestHelper._
import com.github.scli.ParameterExtractor._
import com.github.scli.ParameterModel._
import com.github.scli.ParameterParser.ParametersMap
import com.github.scli.ParametersTestHelper.toParamValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.SortedSet
import scala.util.{Success, Try}

object HelpGeneratorSpec {
  /** The platform-specific line separator. */
  private val CR = System.lineSeparator()

  /** A test column generator function. */
  private val TestColumnGenerator: ColumnGenerator =
    data => List(data.toString)

  /** A test column generator function that returns the option key. */
  private val KeyColumnGenerator: ColumnGenerator =
    data => List(data.key.key)

  /**
   * A test column generator function that returns the multi-line help text.
   */
  private val HelpColumnGenerator: ColumnGenerator =
    data => data.attributes.get(ParameterModel.AttrHelpText).get.split(CR).toList

  /**
   * Creates a new model context with standard settings and the given options.
   *
   * @param params the parameters
   * @return the model context
   */
  private def createModelContext(params: Map[ParameterKey, ParameterAttributes] = Map.empty): ModelContext =
    new ModelContext(params, SortedSet.empty, ParameterModel.EmptyAliasMapping, None, List.empty)

  /**
   * Generates a model context object that contains a number of test options.
   *
   * @param count the number of options to generate
   * @return the test model context
   */
  private def modelContextWithOptions(count: Int): ModelContext = {
    val options = (1 to count).map(testOptionMetaData)
      .map(data => (data.key, data.attributes)).toMap
    createModelContext(options)
  }

  /**
   * Modifies a model context by adding an input parameter to it.
   *
   * @param modelContext the context to be updated
   * @param multiplicity the multiplicity of the parameter
   * @param key          the parameter key
   * @param index        the index of the input parameter
   * @return the updated model context
   */
  private def addInputParameter(modelContext: ModelContext, multiplicity: Multiplicity, key: String = Key.key,
                                index: Int = 1): ModelContext =
    modelContext.addInputParameter(index, Some(key), None)
      .addAttribute(ParameterModel.AttrMultiplicity, multiplicity)

  /**
   * Returns a ''ParameterMetaData'' object for the given key with an attribute
   * map that has the group attribute specified.
   *
   * @param key        the parameter key
   * @param attrGroups the group(s) the key belongs to
   * @return the ''ParameterMetaData'' with this information
   */
  private def parameterDataForGroup(key: ParameterKey, attrGroups: String*): ParameterMetaData = {
    val attributes = new ParameterAttributes + (AttrGroup -> attrGroups.toSet)
    ParameterMetaData(key, attributes)
  }
}

/**
 * Test class for testing whether help and usage texts can be generated
 * correctly from ''CliExtractor'' objects.
 */
class HelpGeneratorSpec extends AnyFlatSpec with Matchers {

  import HelpGeneratorSpec._

  /**
   * Runs an extractor on the parameters specified.
   *
   * @param params the parameters map
   * @param ext    the extractor to execute
   * @return the result of the execution
   */
  private def runExtractor(params: ParametersMap, ext: CliExtractor[Try[String]]):
  (Try[String], ExtractionContext) = {
    val context = ExtractionContext(params, ParameterModel.EmptyModelContext, DummyConsoleReader,
      ParameterManager.defaultExceptionGenerator, None)
    ParameterExtractor.runExtractor(ext, context)
  }

  "The CLI library" should "generate option help texts with default settings" in {
    val Count = 8
    val ExpText = (1 to Count).map(testOptionMetaData).mkString(CR + CR)
    val modelContext = modelContextWithOptions(Count)

    val text = HelpGenerator.generateParametersHelp(modelContext)(TestColumnGenerator)
    text should be(ExpText)
  }

  it should "support changing the newline string" in {
    val Count = 4
    val ExpText = (1 to Count).map(testOptionMetaData).mkString(CR)
    val modelContext = modelContextWithOptions(Count)

    val text = HelpGenerator.generateParametersHelp(modelContext, optNewline = None)(TestColumnGenerator)
    text should be(ExpText)
  }

  it should "sort parameters in a case-insensitive manner" in {
    val Count = 4
    val k1 = testKey(0)
    val k2 = testKey(Count + 1)
    val KeyMin = k1.copy(key = k1.key.toLowerCase(Locale.ROOT))
    val KeyMax = k2.copy(key = k2.key.toUpperCase(Locale.ROOT))
    val ExpText = testOptionMetaData(KeyMin, HelpText + KeyMin).toString + CR + CR +
      (1 to Count).map(testOptionMetaData).mkString(CR + CR) + CR + CR +
      testOptionMetaData(KeyMax, HelpText + KeyMax).toString
    val modelContext = modelContextWithOptions(Count)
      .addOption(KeyMin, Some(HelpText + KeyMin))
      .addOption(KeyMax, Some(HelpText + KeyMax))

    val text = HelpGenerator.generateParametersHelp(modelContext)(TestColumnGenerator)
    text should be(ExpText)
  }

  it should "support a custom sort function for parameters" in {
    val Count = 8
    val sortFunc: ParameterSortFunc = _.sortWith(_.key.key > _.key.key) // reverse sort
    val ExpText = (1 to Count).map(testOptionMetaData).reverse.mkString(CR + CR)
    val modelContext = modelContextWithOptions(Count)

    val text = HelpGenerator.generateParametersHelp(modelContext, sortFunc = sortFunc)(TestColumnGenerator)
    text should be(ExpText)
  }

  it should "support filtering the parameters to generate help information for" in {
    val CountAll = 8
    val CountFiltered = 4
    val ExpText = (1 to CountFiltered).map(testOptionMetaData).mkString(CR + CR)
    val modelContext = modelContextWithOptions(CountAll)
    val filterFunc: ParameterFilter = _.key.key <= testKey(CountFiltered).key

    val text = HelpGenerator.generateParametersHelp(modelContext, filterFunc = filterFunc)(TestColumnGenerator)
    text should be(ExpText)
  }

  it should "support multiple columns for the parameter help" in {
    val modelContext = createModelContext()
      .addOption(Key, Some(HelpText))
    val ExpText = Key.key + HelpGenerator.DefaultPadding + HelpText

    val text = HelpGenerator.generateParametersHelp(modelContext)(KeyColumnGenerator, HelpColumnGenerator)
    text should be(ExpText)
  }

  it should "align the columns for the parameter help based on their maximum length" in {
    val ShortHelpText = "short help"
    val modelContext = createModelContext()
      .addOption(testKey(1), Some(ShortHelpText))
      .addOption(testKey(2), Some(HelpText))
    val ExpText = ShortHelpText + (" " * (HelpText.length - ShortHelpText.length)) +
      HelpGenerator.DefaultPadding + testKey(1).key + CR + CR +
      HelpText + HelpGenerator.DefaultPadding + testKey(2).key

    val text = HelpGenerator.generateParametersHelp(modelContext)(HelpColumnGenerator, KeyColumnGenerator)
    text should be(ExpText)
  }

  it should "support multiple lines in columns for parameter help" in {
    val spaceKey = " " * Key.key.length
    val modelContext = createModelContext()
      .addOption(Key, Some("Line1" + CR + "Line2" + CR + "Line3"))
    val ExpText = Key.key + HelpGenerator.DefaultPadding + "Line1" + CR +
      spaceKey + HelpGenerator.DefaultPadding + "Line2" + CR +
      spaceKey + HelpGenerator.DefaultPadding + "Line3"

    val text = HelpGenerator.generateParametersHelp(modelContext)(KeyColumnGenerator, HelpColumnGenerator)
    text should be(ExpText)
  }

  it should "handle empty cells when generating parameter help" in {
    val EmptyColumnGenerator: ColumnGenerator = _ => List.empty
    val modelContext = createModelContext()
      .addOption(Key, None)
    val ExpText = Key.key + HelpGenerator.DefaultPadding

    val text = HelpGenerator.generateParametersHelp(modelContext)(KeyColumnGenerator, EmptyColumnGenerator)
    text should be(ExpText)
  }

  it should "generate parameters help if no parameter is matched" in {
    val modelContext = createModelContext()
      .addOption(Key, Some(HelpText))
    val filter: ParameterFilter = _ => false

    val text = HelpGenerator.generateParametersHelp(modelContext, filterFunc = filter)(KeyColumnGenerator)
    text should be("")
  }

  it should "support changing the padding string for the parameter help table" in {
    val OtherPadding = " | "
    val modelContext = createModelContext()
      .addOption(Key, Some(HelpText))
    val ExpText = Key.key + OtherPadding + HelpText

    val text = HelpGenerator.generateParametersHelp(modelContext, padding = OtherPadding)(KeyColumnGenerator,
      HelpColumnGenerator)
    text should be(ExpText)
  }

  it should "support rendering multiple help tables" in {
    val cell11 = List("o")
    val cell12 = List("Help text of o line 1", "Help text of o line 2")
    val cell21 = List("someOption")
    val cell22 = List("someOption help")
    val cell31 = List("o2")
    val cell32 = List("This is a very exciting option, which does blah", "and more.")
    val table1: HelpTable = List(List(cell11, cell12), List(cell21, cell22))
    val table2: HelpTable = List(List(cell31, cell32))
    val Padding = "|"
    val NewLine = "<<"

    // Checks a single help table and returns the position of the Padding and the line length
    def checkRenderedTable(result: String, expLineCount: Int, expNewLineAt: Int = -1): (Int, Int) = {
      val lines = result.split(HelpGenerator.CR)
      lines should have length expLineCount
      val col1Length = lines.head.indexOf(Padding)
      col1Length should be > 0
      val lineLength = lines.head.length
      lines forall { line =>
        (line.indexOf(Padding) == col1Length && line.length == lineLength) || line == NewLine
      } shouldBe true
      if (expNewLineAt >= 0) {
        lines(expNewLineAt) should be(NewLine)
      }
      (col1Length, lineLength)
    }

    val texts = HelpGenerator.renderHelpTables(List(table1, table2), Padding, Some(NewLine))
    texts should have size 2
    val (separatorPos1, length1) = checkRenderedTable(texts.head, 4, expNewLineAt = 2)
    val (separatorPos2, length2) = checkRenderedTable(texts(1), 2)
    separatorPos1 should be(separatorPos2)
    length1 should be(length2)
  }

  it should "support rendering multiple help tables including empty ones" in {
    val table1: HelpTable = List(List(List("foo"), List("bar")))
    val table2: HelpTable = Nil

    val texts = HelpGenerator.renderHelpTables(List(table2, table1))
    texts.head should be("")
  }

  it should "provide a special sort function for input parameters" in {
    val Key1 = ParameterKey("source", shortAlias = false, hasPrefix = false)
    val Key2 = ParameterKey("destination", shortAlias = false, hasPrefix = false)
    val Key3 = ParameterKey("flags", shortAlias = false, hasPrefix = false)
    val modelContext = createModelContext()
      .addInputParameter(1, Some(Key1.key), None)
      .addInputParameter(2, Some(Key2.key), None)
      .addInputParameter(3, Some(Key3.key), None)
    val ExpResult = List(ParameterMetaData(Key1, modelContext.options(Key1)),
      ParameterMetaData(Key2, modelContext.options(Key2)),
      ParameterMetaData(Key3, modelContext.options(Key3)))

    val result = HelpGenerator.inputParamSortFunc(modelContext)(modelContext.parameterMetaData.toSeq)
    result should be(ExpResult)
  }

  it should "handle non-input parameters in the special sort function" in {
    val Key1 = ParameterKey("source", shortAlias = false, hasPrefix = false)
    val Key2 = ParameterKey("destination", shortAlias = false, hasPrefix = false)
    val modelContext = createModelContext()
      .addInputParameter(1, Some(Key1.key), None)
      .addInputParameter(2, Some(Key2.key), None)
      .addOption(Key, Some(HelpText))
    val ExpResult = List(ParameterMetaData(Key1, modelContext.options(Key1)),
      ParameterMetaData(Key2, modelContext.options(Key2)),
      ParameterMetaData(Key, modelContext.options(Key)))

    val result = HelpGenerator.inputParamSortFunc(modelContext)(modelContext.parameterMetaData.toSeq)
    result should be(ExpResult)
  }

  it should "provide a filter function that filters for options" in {
    val Key2 = ParameterKey("otherOptionKey", shortAlias = false)
    val modelContext = createModelContext()
      .addOption(Key, Some(HelpText))
      .addInputParameter(1, Some("ignored"), None)
      .addOption(Key2, Some("other help"))
    val ExpResult = List(ParameterMetaData(Key2, modelContext.options(Key2)),
      ParameterMetaData(Key, modelContext.options(Key)))

    val result = modelContext.parameterMetaData.filter(HelpGenerator.OptionsFilterFunc)
    result should contain theSameElementsAs ExpResult
  }

  it should "provide a filter function that filters for input parameters" in {
    val Key1 = Key.copy(hasPrefix = false)
    val Key2 = ParameterKey("testInput", shortAlias = false, hasPrefix = false)
    val modelContext = createModelContext()
      .addInputParameter(1, Some(Key1.key), None)
      .addOption(ParameterKey("ignored", shortAlias = false), None)
      .addInputParameter(2, Some(Key2.key), Some("text"))
    val ExpResult = List(ParameterMetaData(Key2, modelContext.options(Key2)),
      ParameterMetaData(Key1, modelContext.options(Key1)))

    val result = modelContext.parameterMetaData.filter(HelpGenerator.InputParamsFilterFunc)
    result should contain theSameElementsAs ExpResult
  }

  it should "provide a filter function that accepts elements from a given group" in {
    val Group = "TheGroup"
    val modelContext = createModelContext()
      .addOption(ParameterKey("ignored", shortAlias = false), Some("test"))
      .startGroup(Group)
      .addOption(Key, Some(HelpText))
      .endGroup()
      .startGroup("otherGroup")
      .addOption(ParameterKey("ignored2", shortAlias = false), None)
    val ExpResult = ParameterMetaData(Key, modelContext.options(Key))

    val result = modelContext.parameterMetaData.filter(HelpGenerator.groupFilterFunc(Group))
    result should contain only ExpResult
  }

  it should "provide a filter function that accepts elements belonging to multiple groups" in {
    val Group1 = "IncludeGroup1"
    val Group2 = "IncludeGroup2"
    val Key2 = testKey(10)
    val Key3 = testKey(20)
    val modelContext = createModelContext()
      .addOption(ParameterKey("ignored", shortAlias = false), Some("ignored help 1"))
      .startGroup(Group2)
      .addOption(Key2, None)
      .startGroup(Group1)
      .addOption(Key, Some(HelpText))
      .endGroup()
      .endGroup()
      .startGroup(Group1)
      .addOption(Key3, None)
    val ExpResult = List(ParameterMetaData(Key, modelContext.options(Key)),
      ParameterMetaData(Key2, modelContext.options(Key2)),
      ParameterMetaData(Key3, modelContext.options(Key3)))

    val result = modelContext.parameterMetaData.filter(HelpGenerator.groupFilterFunc(Group1, Group2))
    result should contain theSameElementsAs ExpResult
  }

  it should "provide a filter function that accepts elements not assigned to any group" in {
    val modelContext = createModelContext()
      .addOption(Key, Some(HelpText))
      .startGroup("anyGroup")
      .addOption(ParameterKey("ignored", shortAlias = false), Some("test"))
    val ExpResult = ParameterMetaData(Key, modelContext.options(Key))

    val result = modelContext.parameterMetaData.filter(HelpGenerator.UnassignedGroupFilterFunc)
    result should contain only ExpResult
  }

  it should "provide a filter function that filters for a specific attribute" in {
    val modelContext = createModelContext()
      .addOption(Key, Some(HelpText))
      .addOption(ParameterKey("ignored", shortAlias = false), None)
    val ExpResult = ParameterMetaData(Key, modelContext.options(Key))

    val result = modelContext.parameterMetaData
      .filter(HelpGenerator.attributeFilterFunc(ParameterModel.AttrHelpText))
    result should contain only ExpResult
  }

  it should "allow combining filters with AND semantics" in {
    val modelContext = createModelContext()
      .addOption(Key, Some(HelpText))
      .addInputParameter(1, None, None)
      .startGroup("anyGroup")
      .addOption(ParameterKey("ignored", shortAlias = false), Some("foo"))
    val ExpResult = ParameterMetaData(Key, modelContext.options(Key))

    val filter = HelpGenerator.andFilter(HelpGenerator.OptionsFilterFunc,
      HelpGenerator.UnassignedGroupFilterFunc)
    val result = modelContext.parameterMetaData.filter(filter)
    result should contain only ExpResult
  }

  it should "allow combining filters with OR semantics" in {
    val KeyInput = ParameterKey("myInput", shortAlias = false, hasPrefix = false)
    val Group = "ImportantGroup"
    val modelContext = createModelContext()
      .addOption(ParameterKey("ignored", shortAlias = false), Some("foo"))
      .addInputParameter(1, Some(KeyInput.key), None)
      .startGroup(Group)
      .addOption(Key, Some(HelpText))
    val ExpResult = List(ParameterMetaData(KeyInput, modelContext.options(KeyInput)),
      ParameterMetaData(Key, modelContext.options(Key)))

    val filter = HelpGenerator.orFilter(HelpGenerator.InputParamsFilterFunc,
      HelpGenerator.groupFilterFunc(Group))
    val result = modelContext.parameterMetaData.filter(filter)
    result should contain theSameElementsAs ExpResult
  }

  it should "support negating a filter" in {
    val modelContext = createModelContext()
      .addOption(ParameterKey("ignored", shortAlias = false), Some("test"))
      .startGroup("anyGroup")
      .addOption(Key, Some(HelpText))
    val ExpResult = ParameterMetaData(Key, modelContext.options(Key))
    val filter = HelpGenerator.negate(HelpGenerator.UnassignedGroupFilterFunc)

    val result = modelContext.parameterMetaData.filter(filter)
    result should contain only ExpResult
  }

  it should "generate the overview of an input parameter with multiplicity 1..1" in {
    val modelContext = addInputParameter(createModelContext(), Multiplicity.SingleValue)

    val result = HelpGenerator.generateInputParamsOverview(modelContext)
    result should contain only "<" + Key.key + ">"
  }

  it should "generate the overview of an input parameter if the upper bound is > than the lower bound" in {
    val modelContext = addInputParameter(createModelContext(), Multiplicity(1, 3))
    val ExpResult = s"<${Key.key}1> [...<${Key.key}3>]"

    val result = HelpGenerator.generateInputParamsOverview(modelContext)
    result should contain only ExpResult
  }

  it should "generate the overview of an input parameter if the upper bound is the lower bound + 1" in {
    val modelContext = addInputParameter(createModelContext(), Multiplicity(1, 2))
    val ExpResult = s"<${Key.key}1> [<${Key.key}2>]"

    val result = HelpGenerator.generateInputParamsOverview(modelContext)
    result should contain only ExpResult
  }

  it should "generate the overview of an input parameter if the upper bound is unrestricted" in {
    val modelContext = addInputParameter(createModelContext(), Multiplicity(1, -1))
    val ExpResult = s"<${Key.key}1> [<${Key.key}2> ...]"

    val result = HelpGenerator.generateInputParamsOverview(modelContext)
    result should contain only ExpResult
  }

  it should "generate the overview of an input parameter if the lower bound is > 2" in {
    val modelContext = addInputParameter(createModelContext(), Multiplicity(3, -1))
    val ExpResult = s"<${Key.key}1>...<${Key.key}3> [<${Key.key}4> ...]"

    val result = HelpGenerator.generateInputParamsOverview(modelContext)
    result should contain only ExpResult
  }

  it should "generate the overview of an input parameter if the lower bound is 2" in {
    val modelContext = addInputParameter(createModelContext(), Multiplicity(2, 3))
    val ExpResult = s"<${Key.key}1> <${Key.key}2> [<${Key.key}3>]"

    val result = HelpGenerator.generateInputParamsOverview(modelContext)
    result should contain only ExpResult
  }

  it should "generate the overview of an optional input parameter" in {
    val modelContext = addInputParameter(createModelContext(), Multiplicity.Unbounded)
    val ExpResult = s"[<${Key.key}1> <${Key.key}2> ...]"

    val result = HelpGenerator.generateInputParamsOverview(modelContext)
    result should contain only ExpResult
  }

  it should "generate the overview for multiple input parameters" in {
    val Key2 = "source"
    val modelContext = addInputParameter(
      addInputParameter(createModelContext(), Multiplicity.SingleValue, key = Key2),
      Multiplicity(1, -1), index = 2)
    val ExpResult = List(s"<$Key2>", s"<${Key.key}1> [<${Key.key}2> ...]")

    val result = HelpGenerator.generateInputParamsOverview(modelContext)
    result should contain theSameElementsInOrderAs ExpResult
  }

  it should "generate the overview of an input parameter if no multiplicity is available" in {
    val modelContext = createModelContext()
      .addInputParameter(1, Some(Key.key), None)
    val ExpResult = s"[<${Key.key}1> <${Key.key}2> ...]"

    val result = HelpGenerator.generateInputParamsOverview(modelContext)
    result should contain only ExpResult
  }

  it should "support customizing the symbols for the overview of input parameters" in {
    val symbols = InputParamOverviewSymbols(optionalPrefix = "{", optionalSuffix = "}",
      keyPrefix = "(", keySuffix = ")", ellipsis = "_")
    val modelContext = addInputParameter(createModelContext(), Multiplicity(3, -1))
    val ExpResult = s"(${Key.key}1)_(${Key.key}3) {(${Key.key}4) _}"

    val result = HelpGenerator.generateInputParamsOverview(modelContext, symbols)
    result should contain only ExpResult
  }

  it should "add parameter aliases to ParameterMetaData" in {
    val alias1 = ParameterKey("anAlias", shortAlias = false)
    val alias2 = ParameterKey("anotherAlias", shortAlias = true)
    val aliasList = List(alias1, alias2)
    val context = modelContextWithOptions(2)
      .addOption(Key, None)
      .addAlias(alias1)
      .addAlias(alias2)

    val result = context.parameterMetaData.filter { data =>
      data.key == Key && data.aliases == aliasList
    }
    result should have size 1
  }

  it should "de-duplicate aliases of a parameter" in {
    val alias = ParameterKey("a", shortAlias = true)
    val aliasList = List(alias)
    val context = modelContextWithOptions(2)
      .addOption(Key, None)
      .addAlias(alias)
      .addAlias(alias)

    val result = context.parameterMetaData.filter { data =>
      data.key == Key && data.aliases == aliasList
    }
    result should have size 1
  }

  it should "create a group context filter based on extractors" in {
    val Group1 = "myFirstGroup"
    val Group2 = "MyOtherGroup"
    val params = Map(testKey(1) -> List(Group1), testKey(2) -> List(Group2))
    val extrCtx = ExtractionContext(Parameters(toParamValues(params), Set.empty), createModelContext(),
      DefaultConsoleReader, ParameterManager.defaultExceptionGenerator, None)
    val ext1 = optionValue(testKey(1).key).mandatory
    val ext2 = optionValue(testKey(2).key).mandatory
    val extErr = optionValue("undefined").mandatory

    val filter = HelpGenerator.contextGroupFilterForExtractors(extrCtx, List(ext1, ext2, extErr))
    filter(parameterDataForGroup(testKey(1), Group1)) shouldBe true
    filter(parameterDataForGroup(testKey(2), "test", Group2)) shouldBe true
    filter(parameterDataForGroup(testKey(1), "unknown-group")) shouldBe false
    filter(testOptionMetaData(1)) shouldBe true
  }

  it should "create a group context filter that excludes parameters without a group" in {
    val Group = "aGroup"
    val params = toParamValues(Map(Key -> List(Group)))
    val extrCtx = ExtractionContext(Parameters(params, Set.empty), createModelContext(), DefaultConsoleReader,
      ParameterManager.defaultExceptionGenerator, None)
    val ext = optionValue(Key.key).mandatory

    val filter = HelpGenerator.contextGroupFilterForExtractors(extrCtx, List(ext), includeNoGroup = false)
    filter(parameterDataForGroup(Key, Group)) shouldBe true
    filter(testOptionMetaData(1)) shouldBe false
  }

  it should "create a conditional group extractor that handles a true result" in {
    val Group = "if-group"
    val params = toParamValues(Map(Key -> List("true")))
    val extCond = switchValue(Key.key)

    val extGroup = HelpGenerator.conditionalGroupExtractor(extCond, Group)
    val (res, _) = runExtractor(params, extGroup)
    res should be(Success(Group))
  }

  it should "create a conditional group extractor that handles a false result" in {
    val Group = "else-group"
    val params = toParamValues(Map(Key -> List("false")))
    val extCond = switchValue(Key.key)

    val extGroup = HelpGenerator.conditionalGroupExtractor(extCond, "ignore", Some(Group))
    val (res, _) = runExtractor(params, extGroup)
    res should be(Success(Group))
  }

  it should "create a conditional group extractor that handles a false result if the else group is undefined" in {
    val params = toParamValues(Map(Key -> List("false")))
    val extCond = switchValue(Key.key)

    val extGroup = HelpGenerator.conditionalGroupExtractor(extCond, "ignore")
    val (res, _) = runExtractor(params, extGroup)
    res.isSuccess shouldBe false
  }

  it should "create a conditional group extractor that handles a failure result" in {
    val params = toParamValues(Map(Key -> List("not a boolean value")))
    val extCond = switchValue(Key.key)

    val extGroup = HelpGenerator.conditionalGroupExtractor(extCond, "ignore")
    val (res, _) = runExtractor(params, extGroup)
    res.isSuccess shouldBe false
  }
}
