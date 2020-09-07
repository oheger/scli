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

import java.io.IOException

import com.github.scli.HelpGeneratorTestHelper.{HelpText, Key, UndefinedAttribute}
import com.github.scli.ParameterExtractor._
import com.github.scli.ParameterModel.{AttrErrCause, AttrErrOriginalValue, AttrHelpText, AttrMultiplicity, FailureContext, InputParameterRef, ModelContext, ParameterAttributeKey, ParameterAttributes, ParameterKey}
import com.github.scli.ParameterParser.OptionElement
import com.github.scli.ParametersTestHelper.toParamValues
import org.mockito.Mockito.verifyZeroInteractions
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.mockito.MockitoSugar

import scala.collection.SortedSet
import scala.reflect.ClassTag
import scala.util.Success

object ParameterModelSpec {
  /** Test key of a string attribute. */
  private val AttrString = ParameterAttributeKey[String]("strAttr")

  /** Test key of a numeric attribute. */
  private val AttrInt = ParameterAttributeKey[Integer]("intAttr")

  /** Test value for the string attribute. */
  private val StrValue = "this is a test string value"

  /** Test value for the numeric attribute. */
  private val NumValue: Integer = 77

  /** Constant for undefined parameters used per default to run extractors. */
  private val EmptyParameters = Parameters(Map.empty, Set.empty)

  /**
   * Runs the given ''CliExtractor'' and returns the resulting model context.
   *
   * @param ext       the extractor to be executed
   * @param params    the parameters to be used
   * @param optReader optional console reader for the context
   * @return the resulting model context
   */
  private def generateModelContext(ext: CliExtractor[_], params: Parameters = EmptyParameters,
                                   optReader: Option[ConsoleReader] = None): ModelContext = {
    implicit val reader: ConsoleReader = optReader getOrElse DefaultConsoleReader
    val (_, ctx) = ParameterExtractor.runExtractor(ext, params)
    ctx.modelContext
  }

  /**
   * Generates a model context for a default test extractor.
   *
   * @return the model context
   */
  private def generateDefaultModelContext(): ModelContext = {
    val ext = optionValue(Key.key, Some(HelpText))
      .mandatory
    generateModelContext(ext)
  }

  /**
   * Helper function to obtain the value of an attribute of an option. Throws
   * an exception if the option or the attribute is not present.
   *
   * @param modelContext the helper context
   * @param paramKey     the parameter key
   * @param attrKey      the attribute key
   * @return the value of this attribute
   */
  private def fetchAttribute[A <: AnyRef](modelContext: ModelContext, paramKey: ParameterKey,
                                          attrKey: ParameterAttributeKey[A]): A =
    modelContext.options(paramKey).get(attrKey).get
}

/**
 * Test class for classes related to ''ParameterModel''.
 */
class ParameterModelSpec extends AnyFlatSpec with Matchers with MockitoSugar {

  import ParameterModelSpec._

  "ParameterAttributes" should "contain an empty map initially" in {
    val attributes = new ParameterAttributes

    attributes.attributes should have size 0
  }

  it should "support adding attributes" in {
    val attributes = new ParameterAttributes()
      .add(AttrString, StrValue)
      .add(AttrInt, NumValue)

    attributes.attributes should have size 2
    attributes.get(AttrString) should be(Some(StrValue))
    attributes.get(AttrInt) should be(Some(NumValue))
  }

  it should "support adding attributes using the + operator" in {
    val attributes = new ParameterAttributes + (AttrInt -> NumValue)

    attributes.get(AttrInt) should be(Some(NumValue))
  }

  it should "return an empty option when querying an undefined attribute" in {
    val attributes = new ParameterAttributes

    attributes.get(AttrString) should be(None)
  }

  it should "support get with default if the attribute is present" in {
    val attributes = new ParameterAttributes + (AttrString -> StrValue)

    attributes.getOrElse(AttrString, "unexpected") should be(StrValue)
  }

  it should "support get with default if the attribute is not present" in {
    val attributes = new ParameterAttributes

    attributes.getOrElse(AttrInt, NumValue) should be(NumValue)
  }

  it should "support adding the content of two instances" in {
    val attr1 = new ParameterAttributes + (AttrString -> StrValue)
    val attr2 = new ParameterAttributes + (AttrInt -> NumValue)

    val attributes = attr1 addAll attr2
    attributes.attributes should have size 2
    attributes.get(AttrString) should be(Some(StrValue))
    attributes.get(AttrInt) should be(Some(NumValue))
  }

  it should "support adding the content of two instances using the ++ operator" in {
    val attr1 = new ParameterAttributes + (AttrString -> StrValue)
    val attr2 = new ParameterAttributes + (AttrInt -> NumValue)

    val attributes = attr1 ++ attr2
    attributes.attributes should have size 2
    attributes.get(AttrString) should be(Some(StrValue))
    attributes.get(AttrInt) should be(Some(NumValue))
  }

  it should "correctly override attributes when merging two instances" in {
    val attr1 = new ParameterAttributes + (AttrString -> "to be overridden")
    val attr2 = new ParameterAttributes + (AttrString -> StrValue)

    val attributes = attr1 addAll attr2
    attributes.get(AttrString) should be(Some(StrValue))
  }

  "ModelContext" should "store a help text for an option" in {
    val ext = multiOptionValue(Key.key, help = Some(HelpText))

    val modelContext = generateModelContext(ext)
    modelContext.options.keys should contain only Key
    fetchAttribute(modelContext, Key, ParameterModel.AttrHelpText) should be(HelpText)
  }

  it should "store a help text for a switch" in {
    val ext = switchValue(Key.key, optHelp = Some(HelpText))

    val modelContext = generateModelContext(ext)
    modelContext.options.keys should contain only Key
    fetchAttribute(modelContext, Key, ParameterModel.AttrHelpText) should be(HelpText)
  }

  it should "evaluate the shortAlias flag for an option" in {
    val ext = optionValue(Key.key, shortAlias = true)

    val modelContext = generateModelContext(ext)
    modelContext.options.keySet should contain only Key.copy(shortAlias = true)
  }

  it should "evaluate the shortAlias flag for a switch" in {
    val ext = switchValue(Key.key, shortAlias = true)

    val modelContext = generateModelContext(ext)
    modelContext.options.keySet should contain only Key.copy(shortAlias = true)
  }

  it should "support a description for a constant value extractor" in {
    val FallbackDesc = "This is the fallback value."
    val constExtValue: OptionValue[String] = Success(List("foo"))
    val fallbackExt = constantExtractor(constExtValue, Some(FallbackDesc))
    val ext = multiOptionValue(Key.key)
      .fallback(fallbackExt)

    val modelContext = generateModelContext(ext)
    fetchAttribute(modelContext, Key, ParameterModel.AttrFallbackValue) should be(FallbackDesc)
  }

  it should "support a description for constant values" in {
    val ValueDesc = "This is a meaningful default value."
    val valueExt = constantOptionValueWithDesc(Some(ValueDesc), "foo", "bar")
    val ext = multiOptionValue(Key.key)
      .fallback(valueExt)

    val modelContext = generateModelContext(ext)
    fetchAttribute(modelContext, Key, ParameterModel.AttrFallbackValue) should be(ValueDesc)
  }

  it should "support skipping a description for a constant value" in {
    val valueExt = constantOptionValueWithDesc(None, "foo", "bar")
    val ext = multiOptionValue(Key.key)
      .fallback(valueExt)

    val modelContext = generateModelContext(ext)
    modelContext.hasAttribute(Key, ParameterModel.AttrFallbackValue) shouldBe false
  }

  it should "support a description for multi fallback values" in {
    val ValueDesc = "Description of these values."
    val ext = multiOptionValue(Key.key)
      .fallbackValuesWithDesc(Some(ValueDesc), "foo", "bar", "baz")

    val modelContext = generateModelContext(ext)
    fetchAttribute(modelContext, Key, ParameterModel.AttrFallbackValue) should be(ValueDesc)
  }

  it should "support a description for a single fallback value" in {
    val ValueDesc = "Description of this single value."
    val ext = optionValue(Key.key)
      .fallbackValueWithDesc(Some(ValueDesc), "theValue")

    val modelContext = generateModelContext(ext)
    fetchAttribute(modelContext, Key, ParameterModel.AttrFallbackValue) should be(ValueDesc)
  }

  it should "generate a description for constant values" in {
    val Values = List("val1", "val2", "val3")
    val ValueDesc = s"<${Values.head}, ${Values(1)}, ${Values(2)}>"
    val ext = multiOptionValue(Key.key)
      .fallbackValues(Values.head, Values.tail: _*)

    val modelContext = generateModelContext(ext)
    fetchAttribute(modelContext, Key, ParameterModel.AttrFallbackValue) should be(ValueDesc)
  }

  it should "generate a description for a single constant value" in {
    val Value = "test"
    val ext = multiOptionValue(Key.key)
      .fallbackValues(Value)

    val modelContext = generateModelContext(ext)
    fetchAttribute(modelContext, Key, ParameterModel.AttrFallbackValue) should be(Value)
  }

  it should "update the model context with a fallback extractor" in {
    val paramsMap = Map(Key -> List("true"))
    val params = Parameters(toParamValues(paramsMap), Set.empty)
    val ext = multiOptionValue(Key.key)
      .toBoolean
      .fallbackValues(false)

    val modelContext = generateModelContext(ext, params = params)
    fetchAttribute(modelContext, Key, ParameterModel.AttrFallbackValue) should be(false.toString)
  }

  it should "handle an uninitialized model context gracefully" in {
    val ext = CliExtractor(context => {
      (42, context.updateModelContext(AttrHelpText, "success"))
    })

    val modelContext = generateModelContext(ext)
    modelContext.options should have size 0
  }

  it should "handle an uninitialized current key when adding an alias" in {
    val ext = withAlias(constantOptionValue(42), "x")

    val modelContext = generateModelContext(ext)
    modelContext.aliasMapping.aliasesForKey should have size 0
  }

  it should "support descriptions and keys for input parameters" in {
    val Key1 = Key.copy(hasPrefix = false)
    val Key2 = ParameterKey("target", shortAlias = false, hasPrefix = false)
    val Key3 = ParameterKey("sourceFiles", shortAlias = false, hasPrefix = false)
    val Help2 = "The target directory"
    val Help3 = "List of the files to be copied"
    val ExpInputs = List(InputParameterRef(0, Key2), InputParameterRef(1, Key1), InputParameterRef(2, Key3))
    val extInp1 = inputValue(1, optKey = Some(Key.key), optHelp = Some(HelpText))
    val extInp2 = inputValue(0, optKey = Some(Key2.key), optHelp = Some(Help2))
    val extInp3 = inputValues(2, -1, optKey = Some(Key3.key), optHelp = Some(Help3))
    val ext = for {
      i1 <- extInp1
      i2 <- extInp2
      i3 <- extInp3
    } yield List(i1, i2, i3)

    val modelContext = generateModelContext(ext)
    modelContext.options.keySet should contain theSameElementsAs List(Key1, Key2, Key3)
    modelContext.options(Key1).attributes(ParameterModel.AttrHelpText) should be(HelpText)
    modelContext.options(Key2).attributes(ParameterModel.AttrHelpText) should be(Help2)
    modelContext.options(Key3).attributes(ParameterModel.AttrHelpText) should be(Help3)
    modelContext.inputs should contain theSameElementsInOrderAs ExpInputs
  }

  it should "support attributes for input parameters" in {
    val Attr = ParameterAttributeKey[String]("foo")
    val KeyInput = Key.copy(hasPrefix = false)
    val ext = inputValue(1, Some(Key.key))
    val modelContext1 = generateModelContext(ext)

    val modelContext2 = modelContext1.addAttribute(Attr, "bar")
    modelContext2.inputs should contain only InputParameterRef(1, KeyInput)
    val attrs = modelContext2.options(KeyInput)
    attrs.get(Attr) should be(Some("bar"))
  }

  it should "support input parameters with negative indices" in {
    val Key1 = ParameterKey("k1", shortAlias = false, hasPrefix = false)
    val Key2 = ParameterKey("k2", shortAlias = false, hasPrefix = false)
    val Key3 = ParameterKey("k3", shortAlias = false, hasPrefix = false)
    val Key4 = ParameterKey("k4", shortAlias = false, hasPrefix = false)
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

    val modelContext = generateModelContext(ext)
    modelContext.inputs should contain theSameElementsInOrderAs ExpInputs
  }

  it should "generate a key for an input parameter if necessary" in {
    val Index = 17
    val ExpKey = ParameterKey(ParameterModel.KeyInput + Index, shortAlias = false, hasPrefix = false)
    val ext = inputValue(Index)

    val modelContext = generateModelContext(ext)
    modelContext.inputs should contain only InputParameterRef(Index, ExpKey)
  }

  it should "merge the attributes of command line options that are added multiple times" in {
    val ak1 = ParameterAttributeKey[String]("attr1")
    val ak2 = ParameterAttributeKey[String]("attr2")
    val Attrs1 = new ParameterAttributes + (ak1 -> "value1") + (ak2 -> "value2") +
      (ParameterModel.AttrHelpText -> "old help")
    val ExpAttrs = Attrs1.attributes + (ParameterModel.AttrHelpText -> HelpText)
    val modelContext =
      new ModelContext(Map(Key -> Attrs1), SortedSet.empty, ParameterModel.EmptyAliasMapping, None, Nil)

    val nextContext = modelContext.addOption(Key, Some(HelpText))
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

    val modelContext = generateModelContext(ext)
    modelContext.options(Key).attributes(ParameterModel.AttrMultiplicity) should be(Multiplicity.SingleOptional)
    modelContext.hasAttribute(Key2, ParameterModel.AttrMultiplicity) shouldBe false
  }

  it should "support querying a boolean attribute for a non-existing option" in {
    val modelContext = new ModelContext(Map.empty, SortedSet.empty, ParameterModel.EmptyAliasMapping, None, Nil)

    modelContext.hasAttribute(Key, UndefinedAttribute) shouldBe false
  }

  it should "set a multiplicity attribute for mandatory options" in {
    val Key2 = Key.key + "_optional"
    val ext1 = optionValue(Key.key).mandatory
    val ext2 = multiOptionValue(Key2).single
    val ext = for {
      v1 <- ext1
      v2 <- ext2
    } yield List(v1, v2)

    val modelContext = generateModelContext(ext)
    modelContext.options(Key).attributes(ParameterModel.AttrMultiplicity) should be(Multiplicity.SingleValue)
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

    val modelContext = generateModelContext(ext)
    modelContext.hasAttribute(Key, ParameterModel.AttrGroup) shouldBe false
    val attrIf = modelContext.options(ParameterKey("if", shortAlias = false))
    ParameterModel.isInGroup(attrIf, "grp-if") shouldBe true
    attrIf.attributes(ParameterModel.AttrHelpText) should be("help-if")
    val attrElse = modelContext.options(ParameterKey("else", shortAlias = false))
    ParameterModel.isInGroup(attrElse, "grp-else") shouldBe true
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

    val modelContext = generateModelContext(extCase)
    val attrIfNested = modelContext.options(ParameterKey("if-nested", shortAlias = false))
    ParameterModel.isInGroup(attrIfNested, "grp-if-nested") shouldBe true
    ParameterModel.isInGroup(attrIfNested, "grp-if") shouldBe true
    ParameterModel.groups(attrIfNested) should contain only("grp-if-nested", "grp-if")
    attrIfNested.attributes(ParameterModel.AttrHelpText) should be("help-if-nested")
    val attrElseNested = modelContext.options(ParameterKey("else-nested", shortAlias = false))
    ParameterModel.groups(attrElseNested) should contain only("grp-else-nested", "grp-if")
    ParameterModel.isInGroup(attrElseNested, "grp-if-nested") shouldBe false
    val attrElse = modelContext.options(ParameterKey("else", shortAlias = false))
    ParameterModel.groups(attrElse) should contain only "grp-else"
  }

  it should "merge the values of group attributes" in {
    val extCond = multiOptionValue("condition").isDefined
    val extIf = multiOptionValue(Key.key)
    val extElse = multiOptionValue(Key.key)
    val extCase = conditionalOptionValue(extCond, ifExt = extIf, ifGroup = Some("g1"),
      elseExt = extElse, elseGroup = Some("g2"))

    val modelContext = generateModelContext(extCase)
    val attr = modelContext.options(Key)
    ParameterModel.groups(attr) should contain only("g1", "g2")
  }

  it should "handle groups whose name is a prefix of another group" in {
    val attr = new ParameterAttributes + (ParameterModel.AttrGroup -> Set("groupSub"))

    ParameterModel.isInGroup(attr, "group") shouldBe false
  }

  it should "correctly execute a group check if no groups are available" in {
    val attr = new ParameterAttributes

    ParameterModel.isInGroup(attr, "someGroup") shouldBe false
  }

  it should "return the groups of an option if no groups are available" in {
    val attr = new ParameterAttributes

    ParameterModel.groups(attr) should have size 0
  }

  it should "use a dummy console reader when running extractors to get meta data" in {
    val Key2 = ParameterKey("readerOption", shortAlias = false)
    val extCond = optionValue("condition").isDefined
    val extIf = optionValue(Key.key).fallback(consoleReaderValue(Key2.key))
    val extElse = optionValue(Key2.key)
    val extCase = conditionalValue(extCond, ifExt = extIf, ifGroup = Some("g1"),
      elseExt = extElse, elseGroup = Some("g2"))
    val reader = mock[ConsoleReader]

    val modelContext = generateModelContext(extCase, optReader = Some(reader))
    val attr = modelContext.options(Key2)
    ParameterModel.isInGroup(attr, "g2") shouldBe true
    verifyZeroInteractions(reader)
  }

  it should "correctly handle the groups of a conditional group extractor" in {
    val Key2 = ParameterKey("OtherKey", shortAlias = false)
    val extG1 = multiOptionValue(Key.key)
    val extG2 = multiOptionValue(Key2.key)
    val extGroupSel = constantOptionValue("g1").single.mandatory
    val groupMap = Map("g1" -> extG1, "g2" -> extG2)
    val extCondGroup = conditionalGroupValue(extGroupSel, groupMap)

    val modelContext = generateModelContext(extCondGroup)
    ParameterModel.isInGroup(modelContext.options(Key), "g1") shouldBe true
    ParameterModel.isInGroup(modelContext.options(Key2), "g2") shouldBe true
    ParameterModel.isInGroup(modelContext.options(Key2), "g1") shouldBe false
  }

  it should "set the multiplicity attribute if it is defined" in {
    val ext = multiOptionValue(Key.key).multiplicity(1, 4)

    val modelContext = generateModelContext(ext)
    val attr = modelContext.options(Key)
    attr.attributes(ParameterModel.AttrMultiplicity) should be(Multiplicity(1, 4))
  }

  it should "handle an unrestricted multiplicity" in {
    val ext = multiOptionValue(Key.key).multiplicity()

    val modelContext = generateModelContext(ext)
    val attr = modelContext.options(Key)
    attr.attributes(ParameterModel.AttrMultiplicity) should be(Multiplicity.Unbounded)
  }

  it should "set the parameter type attribute for a plain option" in {
    val ext = multiOptionValue(Key.key)

    val modelContext = generateModelContext(ext)
    fetchAttribute(modelContext, Key, ParameterModel.AttrParameterType) should be(ParameterModel.ParameterTypeOption)
  }

  it should "set the parameter type attribute for an input parameter" in {
    val KeyInput = Key.copy(hasPrefix = false)
    val ext = inputValue(1, optKey = Some(Key.key))

    val modelContext = generateModelContext(ext)
    fetchAttribute(modelContext, KeyInput,
      ParameterModel.AttrParameterType) should be(ParameterModel.ParameterTypeInput)
  }

  it should "set the parameter type attribute for a switch" in {
    val ext = switchValue(Key.key)

    val modelContext = generateModelContext(ext)
    fetchAttribute(modelContext, Key, ParameterModel.AttrParameterType) should be(ParameterModel.ParameterTypeSwitch)
  }

  it should "set the present and fallback value attributes for a switch" in {
    val ext = switchValue(Key.key)

    val modelContext = generateModelContext(ext)
    fetchAttribute(modelContext, Key, ParameterModel.AttrFallbackValue) should be("false")
    fetchAttribute(modelContext, Key, ParameterModel.AttrSwitchValue) should be("true")
  }

  it should "set the present and fallback value attributes for a switch with a presence value of false" in {
    val ext = switchValue(Key.key, presentValue = false)

    val modelContext = generateModelContext(ext)
    fetchAttribute(modelContext, Key, ParameterModel.AttrFallbackValue) should be("true")
    fetchAttribute(modelContext, Key, ParameterModel.AttrSwitchValue) should be("false")
  }

  "FailureContext" should "transform each failure to a metadata structure" in {
    val failures = (1 to 8) map { idx =>
      ExtractionFailure(ParameterKey(s"key$idx", shortAlias = false), new Exception, None, null)
    }
    val failureContext = new FailureContext(generateDefaultModelContext(), failures)

    val data = failureContext.parameterMetaData
    data should have size failures.size
    data.map(_.key).toList should contain theSameElementsInOrderAs failures.map(_.key)
  }

  it should "initialize the aliases list correctly" in {
    val element = OptionElement(ParameterKey("e", shortAlias = true), Some("ignored"))
    val failure = ExtractionFailure(Key, new Exception, Some(element), null)
    val failureContext = new FailureContext(generateDefaultModelContext(), List(failure))

    val data = failureContext.parameterMetaData.head
    data.aliases should contain only element.key
  }

  it should "include the attributes from the model context" in {
    val failure = ExtractionFailure(Key, new Exception, None, null)
    val failureContext = new FailureContext(generateDefaultModelContext(), List(failure))

    val data = failureContext.parameterMetaData.head
    data.attributes.get(AttrMultiplicity).get should be(Multiplicity.SingleValue)
  }

  it should "include an attribute for the original value" in {
    val OrgValue = "the original parameter value"
    val element = OptionElement(ParameterKey("e", shortAlias = true), Some(OrgValue))
    val failure = ExtractionFailure(Key, new Exception, Some(element), null)
    val failureContext = new FailureContext(generateDefaultModelContext(), List(failure))

    val data = failureContext.parameterMetaData.head
    data.attributes.get(AttrErrOriginalValue) should be(Some(OrgValue))
  }

  it should "include an attribute for the causing exception" in {
    val exception = new IOException("Something went wrong :-(")
    val failure = ExtractionFailure(Key, exception, None, null)
    val failureContext = new FailureContext(generateDefaultModelContext(), List(failure))

    val data = failureContext.parameterMetaData.head
    data.attributes.get(AttrErrCause) should be(Some(exception))
  }

  it should "remove duplicate errors" in {
    val msg = "An error occurred!"
    val msg2 = "Another error was found."
    val element = OptionElement(ParameterKey("e", shortAlias = true), None)
    val failure1 = ExtractionFailure(Key, new IllegalStateException(msg), None, null)
    val failure2 = ExtractionFailure(Key, new IllegalStateException(msg), Some(element), null)
    val failure3 = ExtractionFailure(Key, new IllegalStateException(msg), None, null)
    val failure4 = ExtractionFailure(Key, new IllegalStateException(msg),
      Some(OptionElement(element.key, Some("a value"))), null)
    val failure5 = ExtractionFailure(Key, new IOException(msg), None, null)
    val failure6 = ExtractionFailure(Key, new IllegalStateException(msg2), None, null)
    val failureContext = new FailureContext(generateDefaultModelContext(),
      List(failure1, failure2, failure3, failure4, failure5, failure6))

    def findFailure[E](key: ParameterKey, errMsg: String = msg)(implicit ct: ClassTag[E]): Unit = {
      val optFailure = failureContext.parameterMetaData
        .find { data =>
          data.key == Key && data.aliases.head == key &&
            data.attributes.get(AttrErrCause).exists(e => e.getClass == ct.runtimeClass && e.getMessage == errMsg)
        }
      optFailure.isDefined shouldBe true
    }

    val failures = failureContext.parameterMetaData
    failures should have size 4
    findFailure[IllegalStateException](Key)
    findFailure[IllegalStateException](element.key)
    findFailure[IllegalStateException](Key, msg2)
    findFailure[IOException](Key)
  }
}
