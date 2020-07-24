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

import com.github.scli.CliHelpGenerator.{ColumnGenerator, InputParamOverviewSymbols, ParameterFilter, ParameterSortFunc}
import com.github.scli.ParameterExtractor._
import com.github.scli.ParameterModel.{InputParameterRef, ModelContext, ParameterAttributes, ParameterKey, ParameterMetaData}
import org.mockito.Mockito._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.mockito.MockitoSugar

import scala.collection.SortedSet
import scala.util.Success

object CliExtractorHelpSpec {
  /** Key for a test option. */
  private val Key = ParameterKey("testOption", shortAlias = false)

  /** A test help text. */
  private val HelpText = "Test help text for the test help option."

  /** The platform-specific line separator. */
  private val CR = System.lineSeparator()

  /** Constant for undefined parameters used per default to run extractors. */
  private val EmptyParameters = Parameters(Map.empty, Set.empty)

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
    data => data.attributes.attributes(ParameterModel.AttrHelpText).split(CR).toList

  /**
   * Runs the given ''CliExtractor'' and returns the resulting model context.
   *
   * @param ext       the extractor to be executed
   * @param params    the parameters to be used
   * @param optReader optional console reader for the context
   * @return the resulting model context
   */
  private def generateHelpContext(ext: CliExtractor[_], params: Parameters = EmptyParameters,
                                  optReader: Option[ConsoleReader] = None): ModelContext = {
    implicit val reader: ConsoleReader = optReader getOrElse DefaultConsoleReader
    val (_, ctx) = ParameterExtractor.runExtractor(ext, params)
    ctx.modelContext
  }

  /**
   * Helper function to obtain the value of an attribute of an option. Throws
   * an exception if the option or the attribute is not present.
   *
   * @param helpContext the helper context
   * @param paramKey    the parameter key
   * @param attrKey     the attribute key
   * @return the value of this attribute
   */
  private def fetchAttribute(helpContext: ModelContext, paramKey: ParameterKey, attrKey: String): String =
    helpContext.options(paramKey).attributes(attrKey)

  /**
   * Generates the key of the test option with the given index.
   *
   * @param idx the index of the test option
   * @return the key for this option
   */
  private def testKey(idx: Int): ParameterKey = ParameterKey(s"$Key$idx", shortAlias = false)

  /**
   * Generates test meta data for an option.
   *
   * @param idx the index of the test option
   * @return the test meta data
   */
  private def testOptionMetaData(idx: Int): ParameterMetaData = {
    val key = testKey(idx)
    testOptionMetaData(key, HelpText + key)
  }

  /**
   * Generates test meta data based on the given parameters.
   *
   * @param key  the parameter key
   * @param help the help text
   * @return the resulting meta data
   */
  private def testOptionMetaData(key: ParameterKey, help: String): ParameterMetaData = {
    val attrs = Map(ParameterModel.AttrHelpText -> help,
      ParameterModel.AttrParameterType -> ParameterModel.ParameterTypeOption)
    ParameterMetaData(key, ParameterAttributes(attrs))
  }

  /**
   * Creates a new model context with standard settings and the given options.
   *
   * @param params the parameters
   * @return the model context
   */
  private def createModelContext(params: Map[ParameterKey, ParameterAttributes] = Map.empty): ModelContext =
    new ModelContext(params, SortedSet.empty, None, List.empty)

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

  private def addInputParameter(helpContext: ModelContext, multiplicity: String, key: String = Key.key,
                                index: Int = 1): ModelContext =
    helpContext.addInputParameter(index, Some(key), None)
      .addAttribute(ParameterModel.AttrMultiplicity, multiplicity)
}

/**
 * Test class for testing whether help and usage texts can be generated
 * correctly from ''CliExtractor'' objects.
 */
class CliExtractorHelpSpec extends AnyFlatSpec with Matchers with MockitoSugar {

  import CliExtractorHelpSpec._

  "The CLI library" should "store a help text for an option" in {
    val ext = multiOptionValue(Key.key, help = Some(HelpText))

    val helpContext = generateHelpContext(ext)
    helpContext.options.keys should contain only Key
    fetchAttribute(helpContext, Key, ParameterModel.AttrHelpText) should be(HelpText)
  }

  it should "store a help text for a switch" in {
    val ext = switchValue(Key.key, optHelp = Some(HelpText))

    val helpContext = generateHelpContext(ext)
    helpContext.options.keys should contain only Key
    fetchAttribute(helpContext, Key, ParameterModel.AttrHelpText) should be(HelpText)
  }

  it should "support a description for a constant value extractor" in {
    val FallbackDesc = "This is the fallback value."
    val constExtValue: OptionValue[String] = Success(List("foo"))
    val fallbackExt = constantExtractor(constExtValue, Some(FallbackDesc))
    val ext = multiOptionValue(Key.key)
      .fallback(fallbackExt)

    val helpContext = generateHelpContext(ext)
    fetchAttribute(helpContext, Key, ParameterModel.AttrFallbackValue) should be(FallbackDesc)
  }

  it should "support a description for constant values" in {
    val ValueDesc = "This is a meaningful default value."
    val valueExt = constantOptionValueWithDesc(Some(ValueDesc), "foo", "bar")
    val ext = multiOptionValue(Key.key)
      .fallback(valueExt)

    val helpContext = generateHelpContext(ext)
    fetchAttribute(helpContext, Key, ParameterModel.AttrFallbackValue) should be(ValueDesc)
  }

  it should "support skipping a description for a constant value" in {
    val valueExt = constantOptionValueWithDesc(None, "foo", "bar")
    val ext = multiOptionValue(Key.key)
      .fallback(valueExt)

    val helpContext = generateHelpContext(ext)
    helpContext.hasAttribute(Key, ParameterModel.AttrFallbackValue) shouldBe false
  }

  it should "support a description for multi fallback values" in {
    val ValueDesc = "Description of these values."
    val ext = multiOptionValue(Key.key)
      .fallbackValuesWithDesc(Some(ValueDesc), "foo", "bar", "baz")

    val helpContext = generateHelpContext(ext)
    fetchAttribute(helpContext, Key, ParameterModel.AttrFallbackValue) should be(ValueDesc)
  }

  it should "support a description for a single fallback value" in {
    val ValueDesc = "Description of this single value."
    val ext = optionValue(Key.key)
      .fallbackValueWithDesc(Some(ValueDesc), "theValue")

    val helpContext = generateHelpContext(ext)
    fetchAttribute(helpContext, Key, ParameterModel.AttrFallbackValue) should be(ValueDesc)
  }

  it should "generate a description for constant values" in {
    val Values = List("val1", "val2", "val3")
    val ValueDesc = s"<${Values.head}, ${Values(1)}, ${Values(2)}>"
    val ext = multiOptionValue(Key.key)
      .fallbackValues(Values.head, Values.tail: _*)

    val helpContext = generateHelpContext(ext)
    fetchAttribute(helpContext, Key, ParameterModel.AttrFallbackValue) should be(ValueDesc)
  }

  it should "generate a description for a single constant value" in {
    val Value = "test"
    val ext = multiOptionValue(Key.key)
      .fallbackValues(Value)

    val helpContext = generateHelpContext(ext)
    fetchAttribute(helpContext, Key, ParameterModel.AttrFallbackValue) should be(Value)
  }

  it should "update the model context with a fallback extractor" in {
    val paramsMap = Map(Key -> List("true"))
    val params = Parameters(paramsMap, Set.empty)
    val ext = multiOptionValue(Key.key)
      .toBoolean
      .fallbackValues(false)

    val helpContext = generateHelpContext(ext, params = params)
    fetchAttribute(helpContext, Key, ParameterModel.AttrFallbackValue) should be(false.toString)
  }

  it should "handle an uninitialized model context gracefully" in {
    val ext = CliExtractor(context => {
      (42, context.updateModelContext("test", "success"))
    })

    val helpContext = generateHelpContext(ext)
    helpContext.options should have size 0
  }

  it should "support descriptions and keys for input parameters" in {
    val Key2 = ParameterKey("target", shortAlias = false)
    val Key3 = ParameterKey("sourceFiles", shortAlias = false)
    val Help2 = "The target directory"
    val Help3 = "List of the files to be copied"
    val ExpInputs = List(InputParameterRef(0, Key2), InputParameterRef(1, Key), InputParameterRef(2, Key3))
    val extInp1 = inputValue(1, optKey = Some(Key.key), optHelp = Some(HelpText))
    val extInp2 = inputValue(0, optKey = Some(Key2.key), optHelp = Some(Help2))
    val extInp3 = inputValues(2, -1, optKey = Some(Key3.key), optHelp = Some(Help3))
    val ext = for {
      i1 <- extInp1
      i2 <- extInp2
      i3 <- extInp3
    } yield List(i1, i2, i3)

    val helpContext = generateHelpContext(ext)
    helpContext.options.keySet should contain theSameElementsAs List(Key, Key2, Key3)
    helpContext.options(Key).attributes(ParameterModel.AttrHelpText) should be(HelpText)
    helpContext.options(Key2).attributes(ParameterModel.AttrHelpText) should be(Help2)
    helpContext.options(Key3).attributes(ParameterModel.AttrHelpText) should be(Help3)
    helpContext.inputs should contain theSameElementsInOrderAs ExpInputs
  }

  it should "support attributes for input parameters" in {
    val ext = inputValue(1, Some(Key.key))
    val helpContext1 = generateHelpContext(ext)

    val helpContext2 = helpContext1.addAttribute("foo", "bar")
    helpContext2.inputs should contain only InputParameterRef(1, Key)
    val attrs = helpContext2.options(Key)
    attrs.attributes("foo") should be("bar")
  }

  it should "support input parameters with negative indices" in {
    val Key1 = ParameterKey("k1", shortAlias = false)
    val Key2 = ParameterKey("k2", shortAlias = false)
    val Key3 = ParameterKey("k3", shortAlias = false)
    val Key4 = ParameterKey("k4", shortAlias = false)
    val ExpInputs = List(InputParameterRef(0, Key1), InputParameterRef(1, Key2),
      InputParameterRef(-2, Key3), InputParameterRef(-1, Key4))
    val extInp1 = inputValue(0, optKey = Some(Key1.key), optHelp = Some(HelpText))
    val extInp2 = inputValue(1, optKey = Some(Key2.key))
    val extInp3 = inputValue(-2, optKey = Some(Key3.key))
    val extInp4 = inputValue(-1, optKey = Some(Key4.key))
    val ext = for {
      i1 <- extInp1
      i4 <- extInp4
      i3 <- extInp3
      i2 <- extInp2
    } yield List(i1, i2, i3, i4)

    val helpContext = generateHelpContext(ext)
    helpContext.inputs should contain theSameElementsInOrderAs ExpInputs
  }

  it should "generate a key for an input parameter if necessary" in {
    val Index = 17
    val ExpKey = ParameterKey(ParameterModel.KeyInput + Index, shortAlias = false)
    val ext = inputValue(Index)

    val helpContext = generateHelpContext(ext)
    helpContext.inputs should contain only InputParameterRef(Index, ExpKey)
  }

  it should "merge the attributes of command line options that are added multiple times" in {
    val Attrs1 = ParameterAttributes(Map("attr1" -> "value1", "attr2" -> "value2",
      ParameterModel.AttrHelpText -> "old help"))
    val ExpAttrs = Attrs1.attributes + (ParameterModel.AttrHelpText -> HelpText)
    val helpContext = new ModelContext(Map(Key -> Attrs1), SortedSet.empty, None, Nil)

    val nextContext = helpContext.addOption(Key, Some(HelpText))
    nextContext.options(Key).attributes should contain allElementsOf ExpAttrs
  }

  it should "set a multiplicity attribute for options with a single value" in {
    val Key2 = ParameterKey(Key.key + "_other", shortAlias = false)
    val ext1 = optionValue(Key.key)
    val ext2 = multiOptionValue(Key2.key)
    val ext = for {
      v1 <- ext1
      v2 <- ext2
    } yield List(v1, v2)

    val helpContext = generateHelpContext(ext)
    helpContext.options(Key).attributes(ParameterModel.AttrMultiplicity) shouldBe "0..1"
    helpContext.hasAttribute(Key2, ParameterModel.AttrMultiplicity) shouldBe false
  }

  it should "support querying a boolean attribute for a non-existing option" in {
    val helpContext = new ModelContext(Map.empty, SortedSet.empty, None, Nil)

    helpContext.hasAttribute(Key, "foo") shouldBe false
  }

  it should "set a multiplicity attribute for mandatory options" in {
    val Key2 = Key.key + "_optional"
    val ext1 = optionValue(Key.key).mandatory
    val ext2 = multiOptionValue(Key2).single
    val ext = for {
      v1 <- ext1
      v2 <- ext2
    } yield List(v1, v2)

    val helpContext = generateHelpContext(ext)
    helpContext.options(Key).attributes(ParameterModel.AttrMultiplicity) shouldBe "1..1"
  }

  it should "support groups for conditional options" in {
    val extCond = multiOptionValue("condition").isDefined
    val extIf = multiOptionValue("if", Some("help-if"))
    val extElse = multiOptionValue("else", Some("help-else"))
    val extOther = multiOptionValue(Key.key, Some(HelpText))
    val extCase = conditionalOptionValue(extCond, extIf, extElse, Some("grp-if"), Some("grp-else"))
    val ext = for {
      v1 <- extCase
      v2 = extOther
    } yield List(v1, v2)

    val helpContext = generateHelpContext(ext)
    helpContext.hasAttribute(Key, ParameterModel.AttrGroup) shouldBe false
    val attrIf = helpContext.options(ParameterKey("if", shortAlias = false))
    CliHelpGenerator.isInGroup(attrIf, "grp-if") shouldBe true
    attrIf.attributes(ParameterModel.AttrHelpText) should be("help-if")
    val attrElse = helpContext.options(ParameterKey("else", shortAlias = false))
    CliHelpGenerator.isInGroup(attrElse, "grp-else") shouldBe true
    attrElse.attributes(ParameterModel.AttrHelpText) should be("help-else")
  }

  it should "support nested conditional groups" in {
    val extCond1 = multiOptionValue("condition1").isDefined
    val extCond2 = multiOptionValue("condition2").isDefined
    val extIfNested = multiOptionValue("if-nested", Some("help-if-nested"))
    val extElseNested = multiOptionValue("else-nested", Some("help-else-nested"))
    val extElse = multiOptionValue("else", Some("help-else"))
    val extCaseNested = conditionalOptionValue(extCond2, extIfNested, extElseNested,
      ifGroup = Some("grp-if-nested"), elseGroup = Some("grp-else-nested"))
    val extCase = conditionalOptionValue(extCond1, extCaseNested, extElse,
      ifGroup = Some("grp-if"), elseGroup = Some("grp-else"))

    val helpContext = generateHelpContext(extCase)
    val attrIfNested = helpContext.options(ParameterKey("if-nested", shortAlias = false))
    CliHelpGenerator.isInGroup(attrIfNested, "grp-if-nested") shouldBe true
    CliHelpGenerator.isInGroup(attrIfNested, "grp-if") shouldBe true
    CliHelpGenerator.groups(attrIfNested) should contain only("grp-if-nested", "grp-if")
    attrIfNested.attributes(ParameterModel.AttrHelpText) should be("help-if-nested")
    val attrElseNested = helpContext.options(ParameterKey("else-nested", shortAlias = false))
    CliHelpGenerator.groups(attrElseNested) should contain only("grp-else-nested", "grp-if")
    CliHelpGenerator.isInGroup(attrElseNested, "grp-if-nested") shouldBe false
    val attrElse = helpContext.options(ParameterKey("else", shortAlias = false))
    CliHelpGenerator.groups(attrElse) should contain only "grp-else"
  }

  it should "merge the values of group attributes" in {
    val extCond = multiOptionValue("condition").isDefined
    val extIf = multiOptionValue(Key.key)
    val extElse = multiOptionValue(Key.key)
    val extCase = conditionalOptionValue(extCond, ifExt = extIf, ifGroup = Some("g1"),
      elseExt = extElse, elseGroup = Some("g2"))

    val helpContext = generateHelpContext(extCase)
    val attr = helpContext.options(Key)
    CliHelpGenerator.groups(attr) should contain only("g1", "g2")
  }

  it should "handle groups whose name is a prefix of another group" in {
    val mapAttr = Map(ParameterModel.AttrGroup -> "groupSub")
    val attr = ParameterAttributes(mapAttr)

    CliHelpGenerator.isInGroup(attr, "group") shouldBe false
  }

  it should "correctly execute a group check if no groups are available" in {
    val attr = ParameterAttributes(Map.empty)

    CliHelpGenerator.isInGroup(attr, "someGroup") shouldBe false
  }

  it should "return the groups of an option if no groups are available" in {
    val attr = ParameterAttributes(Map.empty)

    CliHelpGenerator.groups(attr) should have size 0
  }

  it should "use a dummy console reader when running extractors to get meta data" in {
    val Key2 = ParameterKey("readerOption", shortAlias = false)
    val extCond = optionValue("condition").isDefined
    val extIf = optionValue(Key.key).fallback(consoleReaderValue(Key2.key))
    val extElse = optionValue(Key2.key)
    val extCase = conditionalValue(extCond, ifExt = extIf, ifGroup = Some("g1"),
      elseExt = extElse, elseGroup = Some("g2"))
    val reader = mock[ConsoleReader]

    val helpContext = generateHelpContext(extCase, optReader = Some(reader))
    val attr = helpContext.options(Key2)
    CliHelpGenerator.isInGroup(attr, "g2") shouldBe true
    verifyZeroInteractions(reader)
  }

  it should "correctly handle the groups of a conditional group extractor" in {
    val Key2 = ParameterKey("OtherKey", shortAlias = false)
    val extG1 = multiOptionValue(Key.key)
    val extG2 = multiOptionValue(Key2.key)
    val extGroupSel = constantOptionValue("g1").single.mandatory
    val groupMap = Map("g1" -> extG1, "g2" -> extG2)
    val extCondGroup = conditionalGroupValue(extGroupSel, groupMap)

    val helpContext = generateHelpContext(extCondGroup)
    CliHelpGenerator.isInGroup(helpContext.options(Key), "g1") shouldBe true
    CliHelpGenerator.isInGroup(helpContext.options(Key2), "g2") shouldBe true
    CliHelpGenerator.isInGroup(helpContext.options(Key2), "g1") shouldBe false
  }

  it should "set the multiplicity attribute if it is defined" in {
    val ext = multiOptionValue(Key.key).multiplicity(1, 4)

    val helpContext = generateHelpContext(ext)
    val attr = helpContext.options(Key)
    attr.attributes(ParameterModel.AttrMultiplicity) should be("1..4")
  }

  it should "handle an unrestricted multiplicity" in {
    val ext = multiOptionValue(Key.key).multiplicity()

    val helpContext = generateHelpContext(ext)
    val attr = helpContext.options(Key)
    attr.attributes(ParameterModel.AttrMultiplicity) should be("0..*")
  }

  it should "set the parameter type attribute for a plain option" in {
    val ext = multiOptionValue(Key.key)

    val helpContext = generateHelpContext(ext)
    fetchAttribute(helpContext, Key, ParameterModel.AttrParameterType) should be(ParameterModel.ParameterTypeOption)
  }

  it should "set the parameter type attribute for an input parameter" in {
    val ext = inputValue(1, optKey = Some(Key.key))

    val helpContext = generateHelpContext(ext)
    fetchAttribute(helpContext, Key, ParameterModel.AttrParameterType) should be(ParameterModel.ParameterTypeInput)
  }

  it should "set the parameter type attribute for a switch" in {
    val ext = switchValue(Key.key)

    val helpContext = generateHelpContext(ext)
    fetchAttribute(helpContext, Key, ParameterModel.AttrParameterType) should be(ParameterModel.ParameterTypeSwitch)
  }

  it should "set the present and fallback value attributes for a switch" in {
    val ext = switchValue(Key.key)

    val helpContext = generateHelpContext(ext)
    fetchAttribute(helpContext, Key, ParameterModel.AttrFallbackValue) should be("false")
    fetchAttribute(helpContext, Key, ParameterModel.AttrSwitchValue) should be("true")
  }

  it should "set the present and fallback value attributes for a switch with a presence value of false" in {
    val ext = switchValue(Key.key, presentValue = false)

    val helpContext = generateHelpContext(ext)
    fetchAttribute(helpContext, Key, ParameterModel.AttrFallbackValue) should be("true")
    fetchAttribute(helpContext, Key, ParameterModel.AttrSwitchValue) should be("false")
  }

  it should "generate option help texts with default settings" in {
    val Count = 8
    val ExpText = (1 to Count).map(testOptionMetaData).mkString(CR + CR)
    val helpContext = modelContextWithOptions(Count)

    val text = CliHelpGenerator.generateOptionsHelp(helpContext)(TestColumnGenerator)
    text should be(ExpText)
  }

  it should "support changing the newline string" in {
    val Count = 4
    val ExpText = (1 to Count).map(testOptionMetaData).mkString(CR)
    val helpContext = modelContextWithOptions(Count)

    val text = CliHelpGenerator.generateOptionsHelp(helpContext, optNewline = None)(TestColumnGenerator)
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
    val helpContext = modelContextWithOptions(Count)
      .addOption(KeyMin, Some(HelpText + KeyMin))
      .addOption(KeyMax, Some(HelpText + KeyMax))

    val text = CliHelpGenerator.generateOptionsHelp(helpContext)(TestColumnGenerator)
    text should be(ExpText)
  }

  it should "support a custom sort function for parameters" in {
    val Count = 8
    val sortFunc: ParameterSortFunc = _.sortWith(_.key.key > _.key.key) // reverse sort
    val ExpText = (1 to Count).map(testOptionMetaData).reverse.mkString(CR + CR)
    val helpContext = modelContextWithOptions(Count)

    val text = CliHelpGenerator.generateOptionsHelp(helpContext, sortFunc = sortFunc)(TestColumnGenerator)
    text should be(ExpText)
  }

  it should "support filtering the parameters to generate help information for" in {
    val CountAll = 8
    val CountFiltered = 4
    val ExpText = (1 to CountFiltered).map(testOptionMetaData).mkString(CR + CR)
    val helpContext = modelContextWithOptions(CountAll)
    val filterFunc: ParameterFilter = _.key.key <= testKey(CountFiltered).key

    val text = CliHelpGenerator.generateOptionsHelp(helpContext, filterFunc = filterFunc)(TestColumnGenerator)
    text should be(ExpText)
  }

  it should "support multiple columns for the parameter help" in {
    val helpContext = createModelContext()
      .addOption(Key, Some(HelpText))
    val ExpText = Key.key + CliHelpGenerator.DefaultPadding + HelpText

    val text = CliHelpGenerator.generateOptionsHelp(helpContext)(KeyColumnGenerator, HelpColumnGenerator)
    text should be(ExpText)
  }

  it should "align the columns for the parameter help based on their maximum length" in {
    val ShortHelpText = "short help"
    val helpContext = createModelContext()
      .addOption(testKey(1), Some(ShortHelpText))
      .addOption(testKey(2), Some(HelpText))
    val ExpText = ShortHelpText + (" " * (HelpText.length - ShortHelpText.length)) +
      CliHelpGenerator.DefaultPadding + testKey(1).key + CR + CR +
      HelpText + CliHelpGenerator.DefaultPadding + testKey(2).key

    val text = CliHelpGenerator.generateOptionsHelp(helpContext)(HelpColumnGenerator, KeyColumnGenerator)
    text should be(ExpText)
  }

  it should "support multiple lines in columns for parameter help" in {
    val spaceKey = " " * Key.key.length
    val helpContext = createModelContext()
      .addOption(Key, Some("Line1" + CR + "Line2" + CR + "Line3"))
    val ExpText = Key.key + CliHelpGenerator.DefaultPadding + "Line1" + CR +
      spaceKey + CliHelpGenerator.DefaultPadding + "Line2" + CR +
      spaceKey + CliHelpGenerator.DefaultPadding + "Line3"

    val text = CliHelpGenerator.generateOptionsHelp(helpContext)(KeyColumnGenerator, HelpColumnGenerator)
    text should be(ExpText)
  }

  it should "handle empty cells when generating parameter help" in {
    val EmptyColumnGenerator: ColumnGenerator = _ => List.empty
    val helpContext = createModelContext()
      .addOption(Key, None)
    val ExpText = Key.key + CliHelpGenerator.DefaultPadding

    val text = CliHelpGenerator.generateOptionsHelp(helpContext)(KeyColumnGenerator, EmptyColumnGenerator)
    text should be(ExpText)
  }

  it should "generate parameters help if no parameter is matched" in {
    val helpContext = createModelContext()
      .addOption(Key, Some(HelpText))
    val filter: ParameterFilter = _ => false

    val text = CliHelpGenerator.generateOptionsHelp(helpContext, filterFunc = filter)(KeyColumnGenerator)
    text should be("")
  }

  it should "support changing the padding string for the parameter help table" in {
    val OtherPadding = " | "
    val helpContext = createModelContext()
      .addOption(Key, Some(HelpText))
    val ExpText = Key.key + OtherPadding + HelpText

    val text = CliHelpGenerator.generateOptionsHelp(helpContext, padding = OtherPadding)(KeyColumnGenerator,
      HelpColumnGenerator)
    text should be(ExpText)
  }

  it should "provide an attribute ColumnGenerator that handles undefined attributes" in {
    val data = testOptionMetaData(1)
    val generator = CliHelpGenerator.attributeColumnGenerator(testKey(2).key)

    generator(data) should have size 0
  }

  it should "provide a ColumnGenerator that reads the value of an attribute" in {
    val data = testOptionMetaData(Key, HelpText)
    val generator = CliHelpGenerator.attributeColumnGenerator(ParameterModel.AttrHelpText)

    generator(data) should be(List(HelpText))
  }

  it should "provide a default values ColumnGenerator that returns the original value" in {
    val data = testOptionMetaData(Key, HelpText)
    val generator = CliHelpGenerator.defaultValueColumnGenerator(
      CliHelpGenerator.attributeColumnGenerator(ParameterModel.AttrHelpText), "foo", "bar")

    generator(data) should be(List(HelpText))
  }

  it should "provide a default values ColumnGenerator that returns the defaults if necessary" in {
    val data = testOptionMetaData(1)
    val defaults = List("These", "are", "the", "default", "values")
    val generator = CliHelpGenerator.defaultValueColumnGenerator(
      CliHelpGenerator.attributeColumnGenerator(Key.key), defaults: _*)

    generator(data) should be(defaults)
  }

  it should "provide a prefix generator that adds prefix data to another one" in {
    val PrefixLines = List("A prefix line", "Another prefix line")
    val PrefixText = ">>"
    val ExpResult = PrefixLines ++ List(">>Line1", ">>Line2")
    val orgGenerator: ColumnGenerator = _ => List("Line1", "Line2")

    val generator = CliHelpGenerator.prefixColumnGenerator(orgGenerator, PrefixLines, Some(PrefixText))
    generator(testOptionMetaData(1)) should be(ExpResult)
  }

  it should "provide a prefix generator that only adds prefix lines" in {
    val data = testOptionMetaData(Key, HelpText)
    val PrefixLines = List("", "p")
    val ExpResult = PrefixLines ++ List(HelpText)
    val orgGenerator = CliHelpGenerator.attributeColumnGenerator(ParameterModel.AttrHelpText)

    val generator = CliHelpGenerator.prefixColumnGenerator(orgGenerator, PrefixLines)
    generator(data) should be(ExpResult)
  }

  it should "provide a prefix generator that returns no data if the wrapped generator yields no results" in {
    val data = testOptionMetaData(42)
    val orgGenerator = CliHelpGenerator.attributeColumnGenerator("nonExistingKey")

    val generator = CliHelpGenerator.prefixColumnGenerator(orgGenerator,
      prefixText = Some("prefix"), prefixLines = List("a", "b", "c"))
    generator(data) should have size 0
  }

  it should "provide a ColumnGenerator that composes the results of other generators" in {
    val attributes = Map("a1" -> "v1", "a2" -> "v2", "a3" -> "v3")
    val data = ParameterMetaData(Key, ParameterAttributes(attributes))
    val g1 = CliHelpGenerator.attributeColumnGenerator("a1")
    val g2 = CliHelpGenerator.attributeColumnGenerator("a2")
    val g3 = CliHelpGenerator.attributeColumnGenerator("a3")

    val generator = CliHelpGenerator.composeColumnGenerator(g1, g2, g3)
    val result = generator(data)
    result should be(List("v1", "v2", "v3"))
  }

  it should "provide a ColumnGenerator that does line wrapping" in {
    val text =
      """|Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy
         |eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam
         |sea takimata sanctus est Lore ipsum dolor sit amet.""".stripMargin
    val ExpResult = List("Lorem ipsum dolor sit amet,",
      "consetetur sadipscing elitr,",
      "sed diam nonumy",
      "eirmod tempor invidunt ut",
      "labore et dolore magna",
      "aliquyam erat, sed diam",
      "sea takimata sanctus est Lore",
      "ipsum dolor sit amet."
    )
    val data = testOptionMetaData(Key, text)
    val orgGenerator = CliHelpGenerator.attributeColumnGenerator(ParameterModel.AttrHelpText)

    val generator = CliHelpGenerator.wrapColumnGenerator(orgGenerator, 30)
    generator(data) should be(ExpResult)
  }

  it should "provide a line wrapping ColumnGenerator that handles unbreakable strings" in {
    val text = "supercalifragilisticexpialidocious"
    val ExpResult = List("supercalifragilisticexpialidoc", "ious")
    val data = testOptionMetaData(Key, text)
    val orgGenerator = CliHelpGenerator.attributeColumnGenerator(ParameterModel.AttrHelpText)

    val generator = CliHelpGenerator.wrapColumnGenerator(orgGenerator, 30)
    generator(data) should be(ExpResult)
  }

  it should "provide a line wrapping ColumnGenerator that handles line continuation characters" in {
    val text =
      """|Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy \
         |eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam \
         |sea takimata sanctus est Lorem ipsum dolor sit amet.""".stripMargin
    val ExpResult = List(
      "Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed",
      "diam nonumy eirmod tempor invidunt ut labore et dolore magna",
      "aliquyam erat, sed diam sea takimata sanctus est Lorem ipsum",
      "dolor sit amet."
    )
    val data = testOptionMetaData(Key, text)
    val orgGenerator = CliHelpGenerator.attributeColumnGenerator(ParameterModel.AttrHelpText)

    val generator = CliHelpGenerator.wrapColumnGenerator(orgGenerator, 60)
    generator(data) should be(ExpResult)
  }

  it should "provide a ColumnGenerator for the parameter name" in {
    val data = testOptionMetaData(Key, HelpText)

    val generator = CliHelpGenerator.parameterNameColumnGenerator()
    generator(data) should contain only (CliHelpGenerator.DefaultOptionPrefix + Key)
  }

  it should "support customizing the prefixes for the parameter name column generator" in {
    val Prefix = "/"
    val data = testOptionMetaData(Key, HelpText)

    val generator = CliHelpGenerator.parameterNameColumnGenerator(Prefix)
    generator(data) should contain only (Prefix + Key)
  }

  it should "provide a ColumnGenerator for the multiplicity of command line parameters" in {
    val Multiplicity = "many"
    val attributes = Map(ParameterModel.AttrMultiplicity -> Multiplicity)
    val data = ParameterMetaData(Key, ParameterAttributes(attributes))

    val generator = CliHelpGenerator.multiplicityColumnGenerator
    generator(data) should contain only Multiplicity
  }

  it should "provide a multiplicity ColumnGenerator that uses the correct default multiplicity" in {
    val data = testOptionMetaData(1)

    val generator = CliHelpGenerator.multiplicityColumnGenerator
    generator(data) should contain only Multiplicity.UnspecifiedMultiplicityString
  }

  it should "provide a special sort function for input parameters" in {
    val Key1 = ParameterKey("source", shortAlias = false)
    val Key2 = ParameterKey("destination", shortAlias = false)
    val Key3 = ParameterKey("flags", shortAlias = false)
    val helpContext = createModelContext()
      .addInputParameter(1, Some(Key1.key), None)
      .addInputParameter(2, Some(Key2.key), None)
      .addInputParameter(3, Some(Key3.key), None)
    val ExpResult = List(ParameterMetaData(Key1, helpContext.options(Key1)),
      ParameterMetaData(Key2, helpContext.options(Key2)),
      ParameterMetaData(Key3, helpContext.options(Key3)))

    val result = CliHelpGenerator.inputParamSortFunc(helpContext)(helpContext.optionMetaData.toSeq)
    result should be(ExpResult)
  }

  it should "handle non-input parameters in the special sort function" in {
    val Key1 = ParameterKey("source", shortAlias = false)
    val Key2 = ParameterKey("destination", shortAlias = false)
    val helpContext = createModelContext()
      .addInputParameter(1, Some(Key1.key), None)
      .addInputParameter(2, Some(Key2.key), None)
      .addOption(Key, Some(HelpText))
    val ExpResult = List(ParameterMetaData(Key1, helpContext.options(Key1)),
      ParameterMetaData(Key2, helpContext.options(Key2)),
      ParameterMetaData(Key, helpContext.options(Key)))

    val result = CliHelpGenerator.inputParamSortFunc(helpContext)(helpContext.optionMetaData.toSeq)
    result should be(ExpResult)
  }

  it should "provide a filter function that filters for options" in {
    val Key2 = ParameterKey("otherOptionKey", shortAlias = false)
    val helpContext = createModelContext()
      .addOption(Key, Some(HelpText))
      .addInputParameter(1, Some("ignored"), None)
      .addOption(Key2, Some("other help"))
    val ExpResult = List(ParameterMetaData(Key2, helpContext.options(Key2)),
      ParameterMetaData(Key, helpContext.options(Key)))

    val result = helpContext.optionMetaData.filter(CliHelpGenerator.OptionsFilterFunc)
    result should contain theSameElementsAs ExpResult
  }

  it should "provide a filter function that filters for input parameters" in {
    val Key2 = ParameterKey("testInput", shortAlias = false)
    val helpContext = createModelContext()
      .addInputParameter(1, Some(Key.key), None)
      .addOption(ParameterKey("ignored", shortAlias = false), None)
      .addInputParameter(2, Some(Key2.key), Some("text"))
    val ExpResult = List(ParameterMetaData(Key2, helpContext.options(Key2)),
      ParameterMetaData(Key, helpContext.options(Key)))

    val result = helpContext.optionMetaData.filter(CliHelpGenerator.InputParamsFilterFunc)
    result should contain theSameElementsAs ExpResult
  }

  it should "provide a filter function that accepts elements from a given group" in {
    val Group = "TheGroup"
    val helpContext = createModelContext()
      .addOption(ParameterKey("ignored", shortAlias = false), Some("test"))
      .startGroup(Group)
      .addOption(Key, Some(HelpText))
      .endGroup()
      .startGroup("otherGroup")
      .addOption(ParameterKey("ignored2", shortAlias = false), None)
    val ExpResult = ParameterMetaData(Key, helpContext.options(Key))

    val result = helpContext.optionMetaData.filter(CliHelpGenerator.groupFilterFunc(Group))
    result should contain only ExpResult
  }

  it should "provide a filter function that accepts elements belonging to multiple groups" in {
    val Group1 = "IncludeGroup1"
    val Group2 = "IncludeGroup2"
    val helpContext = createModelContext()
      .addOption(ParameterKey("ignored1", shortAlias = false), Some("ignored help 1"))
      .startGroup(Group2)
      .addOption(ParameterKey("ignored2", shortAlias = false), None)
      .startGroup(Group1)
      .addOption(Key, Some(HelpText))
    val ExpResult = ParameterMetaData(Key, helpContext.options(Key))

    val result = helpContext.optionMetaData.filter(CliHelpGenerator.groupFilterFunc(Group1, Group2))
    result should contain only ExpResult
  }

  it should "provide a filter function that accepts elements not assigned to any group" in {
    val helpContext = createModelContext()
      .addOption(Key, Some(HelpText))
      .startGroup("anyGroup")
      .addOption(ParameterKey("ignored", shortAlias = false), Some("test"))
    val ExpResult = ParameterMetaData(Key, helpContext.options(Key))

    val result = helpContext.optionMetaData.filter(CliHelpGenerator.UnassignedGroupFilterFunc)
    result should contain only ExpResult
  }

  it should "provide a filter function that filters for a specific attribute" in {
    val helpContext = createModelContext()
      .addOption(Key, Some(HelpText))
      .addOption(ParameterKey("ignored", shortAlias = false), None)
    val ExpResult = ParameterMetaData(Key, helpContext.options(Key))

    val result = helpContext.optionMetaData
      .filter(CliHelpGenerator.attributeFilterFunc(ParameterModel.AttrHelpText))
    result should contain only ExpResult
  }

  it should "allow combining filters with AND semantics" in {
    val helpContext = createModelContext()
      .addOption(Key, Some(HelpText))
      .addInputParameter(1, None, None)
      .startGroup("anyGroup")
      .addOption(ParameterKey("ignored", shortAlias = false), Some("foo"))
    val ExpResult = ParameterMetaData(Key, helpContext.options(Key))

    val filter = CliHelpGenerator.andFilter(CliHelpGenerator.OptionsFilterFunc,
      CliHelpGenerator.UnassignedGroupFilterFunc)
    val result = helpContext.optionMetaData.filter(filter)
    result should contain only ExpResult
  }

  it should "allow combining filters with OR semantics" in {
    val KeyInput = ParameterKey("myInput", shortAlias = false)
    val Group = "ImportantGroup"
    val helpContext = createModelContext()
      .addOption(ParameterKey("ignored", shortAlias = false), Some("foo"))
      .addInputParameter(1, Some(KeyInput.key), None)
      .startGroup(Group)
      .addOption(Key, Some(HelpText))
    val ExpResult = List(ParameterMetaData(KeyInput, helpContext.options(KeyInput)),
      ParameterMetaData(Key, helpContext.options(Key)))

    val filter = CliHelpGenerator.orFilter(CliHelpGenerator.InputParamsFilterFunc,
      CliHelpGenerator.groupFilterFunc(Group))
    val result = helpContext.optionMetaData.filter(filter)
    result should contain theSameElementsAs ExpResult
  }

  it should "support negating a filter" in {
    val helpContext = createModelContext()
      .addOption(ParameterKey("ignored", shortAlias = false), Some("test"))
      .startGroup("anyGroup")
      .addOption(Key, Some(HelpText))
    val ExpResult = ParameterMetaData(Key, helpContext.options(Key))
    val filter = CliHelpGenerator.negate(CliHelpGenerator.UnassignedGroupFilterFunc)

    val result = helpContext.optionMetaData.filter(filter)
    result should contain only ExpResult
  }

  it should "generate the overview of an input parameter with multiplicity 1..1" in {
    val helpContext = addInputParameter(createModelContext(), "1..1")

    val result = CliHelpGenerator.generateInputParamsOverview(helpContext)
    result should contain only "<" + Key.key + ">"
  }

  it should "generate the overview of an input parameter if the upper bound is > than the lower bound" in {
    val helpContext = addInputParameter(createModelContext(), "1..3")
    val ExpResult = s"<${Key.key}1> [...<${Key.key}3>]"

    val result = CliHelpGenerator.generateInputParamsOverview(helpContext)
    result should contain only ExpResult
  }

  it should "generate the overview of an input parameter if the upper bound is the lower bound + 1" in {
    val helpContext = addInputParameter(createModelContext(), "1..2")
    val ExpResult = s"<${Key.key}1> [<${Key.key}2>]"

    val result = CliHelpGenerator.generateInputParamsOverview(helpContext)
    result should contain only ExpResult
  }

  it should "generate the overview of an input parameter if the upper bound is unrestricted" in {
    val helpContext = addInputParameter(createModelContext(), "1..*")
    val ExpResult = s"<${Key.key}> [...]"

    val result = CliHelpGenerator.generateInputParamsOverview(helpContext)
    result should contain only ExpResult
  }

  it should "generate the overview of an input parameter if the lower bound is > 2" in {
    val helpContext = addInputParameter(createModelContext(), "3..*")
    val ExpResult = s"<${Key.key}1>...<${Key.key}3> [...]"

    val result = CliHelpGenerator.generateInputParamsOverview(helpContext)
    result should contain only ExpResult
  }

  it should "generate the overview of an input parameter if the lower bound is 2" in {
    val helpContext = addInputParameter(createModelContext(), "2..3")
    val ExpResult = s"<${Key.key}1> <${Key.key}2> [<${Key.key}3>]"

    val result = CliHelpGenerator.generateInputParamsOverview(helpContext)
    result should contain only ExpResult
  }

  it should "generate the overview of an optional input parameter" in {
    val helpContext = addInputParameter(createModelContext(), "0..*")
    val ExpResult = s"[<${Key.key}> ...]"

    val result = CliHelpGenerator.generateInputParamsOverview(helpContext)
    result should contain only ExpResult
  }

  it should "generate the overview for multiple input parameters" in {
    val Key2 = "source"
    val helpContext = addInputParameter(
      addInputParameter(createModelContext(), "1..1", key = Key2),
      "1..*", index = 2)
    val ExpResult = List(s"<$Key2>", s"<${Key.key}> [...]")

    val result = CliHelpGenerator.generateInputParamsOverview(helpContext)
    result should contain theSameElementsInOrderAs ExpResult
  }

  it should "generate the overview of an input parameter if no multiplicity is available" in {
    val helpContext = createModelContext()
      .addInputParameter(1, Some(Key.key), None)
    val ExpResult = s"[<${Key.key}> ...]"

    val result = CliHelpGenerator.generateInputParamsOverview(helpContext)
    result should contain only ExpResult
  }

  it should "support customizing the symbols for the overview of input parameters" in {
    val symbols = InputParamOverviewSymbols(optionalPrefix = "{", optionalSuffix = "}",
      keyPrefix = "(", keySuffix = ")", ellipsis = "_")
    val helpContext = addInputParameter(createModelContext(), "3..*")
    val ExpResult = s"(${Key.key}1)_(${Key.key}3) {_}"

    val result = CliHelpGenerator.generateInputParamsOverview(helpContext, symbols)
    result should contain only ExpResult
  }
}
