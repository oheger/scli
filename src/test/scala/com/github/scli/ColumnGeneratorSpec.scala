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

import com.github.scli.HelpGenerator.ColumnGenerator
import com.github.scli.ParameterModel.{ParameterAttributes, ParameterKey, ParameterMetaData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Test class for the several column generators provided by the help generator
 * module.
 */
class ColumnGeneratorSpec extends AnyFlatSpec with Matchers {

  import HelpGeneratorTestHelper._

  "HelpGenerator" should "provide an attribute ColumnGenerator that handles undefined attributes" in {
    val data = testOptionMetaData(1)
    val generator = HelpGenerator.attributeColumnGenerator(testKey(2).key)

    generator(data) should have size 0
  }

  it should "provide a ColumnGenerator that reads the value of an attribute" in {
    val data = testOptionMetaData(Key, HelpText)
    val generator = HelpGenerator.attributeColumnGenerator(ParameterModel.AttrHelpText)

    generator(data) should be(List(HelpText))
  }

  it should "provide a default values ColumnGenerator that returns the original value" in {
    val data = testOptionMetaData(Key, HelpText)
    val generator = HelpGenerator.defaultValueColumnGenerator(
      HelpGenerator.attributeColumnGenerator(ParameterModel.AttrHelpText), "foo", "bar")

    generator(data) should be(List(HelpText))
  }

  it should "provide a default values ColumnGenerator that returns the defaults if necessary" in {
    val data = testOptionMetaData(1)
    val defaults = List("These", "are", "the", "default", "values")
    val generator = HelpGenerator.defaultValueColumnGenerator(
      HelpGenerator.attributeColumnGenerator(Key.key), defaults: _*)

    generator(data) should be(defaults)
  }

  it should "provide a prefix generator that adds a prefix text to the lines of another one" in {
    val PrefixText = ">>"
    val ExpResult = List(">>Line1", ">>Line2")
    val orgGenerator: ColumnGenerator = _ => List("Line1", "Line2")

    val generator = HelpGenerator.prefixTextColumnGenerator(orgGenerator, PrefixText)
    generator(testOptionMetaData(1)) should be(ExpResult)
  }

  it should "provide a prefix generator that adds prefix lines to another one" in {
    val data = testOptionMetaData(Key, HelpText)
    val PrefixLines = List("", "p")
    val ExpResult = PrefixLines ++ List(HelpText)
    val orgGenerator = HelpGenerator.attributeColumnGenerator(ParameterModel.AttrHelpText)

    val generator = HelpGenerator.prefixLinesColumnGenerator(orgGenerator, PrefixLines)
    generator(data) should be(ExpResult)
  }

  it should "provide a prefix generator that returns no data if the wrapped generator yields no results" in {
    val data = testOptionMetaData(42)
    val orgGenerator = HelpGenerator.attributeColumnGenerator("nonExistingKey")

    val generator = HelpGenerator.prefixLinesColumnGenerator(orgGenerator, List("a", "b", "c"))
    generator(data) should have size 0
  }

  it should "provide a prefix generator that adds a generated prefix" in {
    val AttrPrefix = "thePrefix"
    val attributes = Map(AttrPrefix -> "=>")
    val data = ParameterMetaData(Key, ParameterAttributes(attributes))
    val orgGenerator: ColumnGenerator = _ => List("Line1", "Line2")
    val prefixGenerator = HelpGenerator.attributeColumnGenerator(AttrPrefix)
    val ExpResult = List("=>Line1", "=>Line2")

    val generator = HelpGenerator.prefixGeneratedColumnGenerator(orgGenerator, prefixGenerator)
    generator(data) should be(ExpResult)
  }

  it should "provide a generated prefix generator that handles empty output from the wrapped generator" in {
    val orgGenerator = HelpGenerator.attributeColumnGenerator("nonExisting")
    val prefixGenerator: ColumnGenerator = _ => throw new UnsupportedOperationException("Unexpected call")

    val generator = HelpGenerator.prefixGeneratedColumnGenerator(orgGenerator, prefixGenerator)
    generator(testOptionMetaData(1)) should have size 0
  }

  it should "provide a generated prefix generator that handles empty output from the prefix generator" in {
    val Value = "someValue"
    val Attr = "DataAttribute"
    val attributes = Map(Attr -> Value)
    val data = ParameterMetaData(Key, ParameterAttributes(attributes))
    val orgGenerator = HelpGenerator.attributeColumnGenerator(Attr)
    val prefixGenerator = HelpGenerator.attributeColumnGenerator("nonExisting")

    val generator = HelpGenerator.prefixGeneratedColumnGenerator(orgGenerator, prefixGenerator)
    generator(data) should contain only Value
  }

  it should "provide a suffix generator that adds a suffix text to the lines of another one" in {
    val SuffixText = "<<"
    val ExpResult = List("Line1<<", "Line2<<")
    val orgGenerator: ColumnGenerator = _ => List("Line1", "Line2")

    val generator = HelpGenerator.suffixTextColumnGenerator(orgGenerator, SuffixText)
    generator(testOptionMetaData(1)) should be(ExpResult)
  }

  it should "provide a suffix generator that adds suffix lines to another one" in {
    val data = testOptionMetaData(Key, HelpText)
    val SuffixLines = List("", "p")
    val ExpResult = List(HelpText) ++ SuffixLines
    val orgGenerator = HelpGenerator.attributeColumnGenerator(ParameterModel.AttrHelpText)

    val generator = HelpGenerator.suffixLinesColumnGenerator(orgGenerator, SuffixLines)
    generator(data) should be(ExpResult)
  }

  it should "provide a suffix generator that returns no data if the wrapped generator yields no results" in {
    val data = testOptionMetaData(42)
    val orgGenerator = HelpGenerator.attributeColumnGenerator("nonExistingKey")

    val generator = HelpGenerator.suffixLinesColumnGenerator(orgGenerator, List("a", "b", "c"))
    generator(data) should have size 0
  }

  it should "provide a suffix generator that adds a generated suffix" in {
    val AttrSuffix = "thePrefix"
    val attributes = Map(AttrSuffix -> "<=")
    val data = ParameterMetaData(Key, ParameterAttributes(attributes))
    val orgGenerator: ColumnGenerator = _ => List("Line1", "Line2")
    val suffixGenerator = HelpGenerator.attributeColumnGenerator(AttrSuffix)
    val ExpResult = List("Line1<=", "Line2<=")

    val generator = HelpGenerator.suffixGeneratedColumnGenerator(orgGenerator, suffixGenerator)
    generator(data) should be(ExpResult)
  }

  it should "provide a generated suffix generator that handles empty output from the wrapped generator" in {
    val orgGenerator = HelpGenerator.attributeColumnGenerator("nonExisting")
    val suffixGenerator: ColumnGenerator = _ => throw new UnsupportedOperationException("Unexpected call")

    val generator = HelpGenerator.suffixGeneratedColumnGenerator(orgGenerator, suffixGenerator)
    generator(testOptionMetaData(1)) should have size 0
  }

  it should "provide a generated suffix generator that handles empty output from the suffix generator" in {
    val Value = "someValue"
    val Attr = "DataAttribute"
    val attributes = Map(Attr -> Value)
    val data = ParameterMetaData(Key, ParameterAttributes(attributes))
    val orgGenerator = HelpGenerator.attributeColumnGenerator(Attr)
    val suffixGenerator = HelpGenerator.attributeColumnGenerator("nonExisting")

    val generator = HelpGenerator.suffixGeneratedColumnGenerator(orgGenerator, suffixGenerator)
    generator(data) should contain only Value
  }

  it should "provide a ColumnGenerator that composes the results of other generators" in {
    val attributes = Map("a1" -> "v1", "a2" -> "v2", "a3" -> "v3")
    val data = ParameterMetaData(Key, ParameterAttributes(attributes))
    val g1 = HelpGenerator.attributeColumnGenerator("a1")
    val g2 = HelpGenerator.attributeColumnGenerator("a2")
    val g3 = HelpGenerator.attributeColumnGenerator("a3")

    val generator = HelpGenerator.composeColumnGenerator(g1, g2, g3)
    val result = generator(data)
    result should be(List("v1", "v2", "v3"))
  }

  it should "provide a ColumnGenerator that composes a line from two generators" in {
    val attributes = Map("a1" -> "v1", "a2" -> "v2")
    val data = ParameterMetaData(Key, ParameterAttributes(attributes))
    val g1 = HelpGenerator.attributeColumnGenerator("a1")
    val g2 = HelpGenerator.attributeColumnGenerator("a2")

    val generator = HelpGenerator.composeSingleLineColumnGenerator(g1, g2, ", ")
    val result = generator(data)
    result should be(List("v1, v2"))
  }

  it should "provide a ColumnGenerator that composes a line if generator 2 does not produce a result" in {
    val attributes = Map("a1" -> "v1")
    val data = ParameterMetaData(Key, ParameterAttributes(attributes))
    val g1 = HelpGenerator.attributeColumnGenerator("a1")
    val g2 = HelpGenerator.attributeColumnGenerator("a2")

    val generator = HelpGenerator.composeSingleLineColumnGenerator(g1, g2, ", ")
    val result = generator(data)
    result should be(List("v1"))
  }

  it should "provide a ColumnGenerator that composes a line if generator 1 does not produce a result" in {
    val attributes = Map("a1" -> "v1")
    val data = ParameterMetaData(Key, ParameterAttributes(attributes))
    val g1 = HelpGenerator.attributeColumnGenerator("a1")
    val g2 = HelpGenerator.attributeColumnGenerator("a2")

    val generator = HelpGenerator.composeSingleLineColumnGenerator(g2, g1, ", ")
    val result = generator(data)
    result should be(List("v1"))
  }

  it should "provide a ColumnGenerator that composes a line if both generators do not produce a result" in {
    val data = ParameterMetaData(Key, ParameterAttributes(Map.empty))
    val g1 = HelpGenerator.attributeColumnGenerator("a1")
    val g2 = HelpGenerator.attributeColumnGenerator("a2")

    val generator = HelpGenerator.composeSingleLineColumnGenerator(g2, g1, "*")
    val result = generator(data)
    result should have size 0
  }

  it should "provide a ColumnGenerator that appends a separator and handles an empty result" in {
    val data = ParameterMetaData(Key, ParameterAttributes(Map.empty))
    val gen = HelpGenerator.attributeColumnGenerator("someKey")

    val generator = HelpGenerator.separatorColumnGenerator(gen, "---")
    val result = generator(data)
    result should have size 0
  }

  it should "provide a ColumnGenerator that appends a separator to a single-line result" in {
    val Attr = "testAttr"
    val Value = "aValue"
    val data = ParameterMetaData(Key, ParameterAttributes(Map(Attr -> Value)))
    val gen = HelpGenerator.attributeColumnGenerator(Attr)

    val generator = HelpGenerator.separatorColumnGenerator(gen, "***")
    val result = generator(data)
    result should contain only Value
  }

  it should "provide a ColumnGenerator that appends a separator to a multi-line result" in {
    val Attr = "multiLineAttr"
    val data = ParameterMetaData(Key, ParameterAttributes(Map(Attr -> "v1,v2,v3")))
    val gen: ColumnGenerator = data => data.attributes.attributes(Attr).split(",").toList

    val generator = HelpGenerator.separatorColumnGenerator(gen, ";")
    val result = generator(data)
    result should be(List("v1;", "v2;", "v3"))
  }

  it should "provide a ColumnGenerator that converts a result to a single line" in {
    val Attr = "multiLineAttr"
    val data = ParameterMetaData(Key, ParameterAttributes(Map(Attr -> "v1,v2,v3")))
    val gen: ColumnGenerator = data => data.attributes.attributes(Attr).split(",").toList

    val generator = HelpGenerator.singleLineColumnGenerator(gen, "; ")
    val result = generator(data)
    result should contain only "v1; v2; v3"
  }

  it should "provide a ColumnGenerator that converts a result to a single line and handles an empty result" in {
    val data = ParameterMetaData(Key, ParameterAttributes(Map.empty))
    val gen = HelpGenerator.attributeColumnGenerator("undefined")

    val generator = HelpGenerator.singleLineColumnGenerator(gen, "~~~")
    val result = generator(data)
    result should have size 0
  }

  it should "provide a ColumnGenerator that converts a result to a single line and handles a single line" in {
    val Attr = "singleLineAttr"
    val Value = "foo"
    val data = ParameterMetaData(Key, ParameterAttributes(Map(Attr -> Value)))
    val gen = HelpGenerator.attributeColumnGenerator(Attr)

    val generator = HelpGenerator.singleLineColumnGenerator(gen, "===")
    val result = generator(data)
    result should contain only Value
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
    val orgGenerator = HelpGenerator.attributeColumnGenerator(ParameterModel.AttrHelpText)

    val generator = HelpGenerator.wrapColumnGenerator(orgGenerator, 30)
    generator(data) should be(ExpResult)
  }

  it should "provide a line wrapping ColumnGenerator that handles unbreakable strings" in {
    val text = "supercalifragilisticexpialidocious"
    val ExpResult = List("supercalifragilisticexpialidoc", "ious")
    val data = testOptionMetaData(Key, text)
    val orgGenerator = HelpGenerator.attributeColumnGenerator(ParameterModel.AttrHelpText)

    val generator = HelpGenerator.wrapColumnGenerator(orgGenerator, 30)
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
    val orgGenerator = HelpGenerator.attributeColumnGenerator(ParameterModel.AttrHelpText)

    val generator = HelpGenerator.wrapColumnGenerator(orgGenerator, 60)
    generator(data) should be(ExpResult)
  }

  it should "provide a ColumnGenerator for the parameter name" in {
    val data = testOptionMetaData(Key, HelpText)

    val generator = HelpGenerator.parameterNameColumnGenerator()
    generator(data) should contain only (HelpGenerator.DefaultLongOptionPrefix + Key.key)
  }

  it should "provide a ColumnGenerator that outputs the correct prefix for the parameter name" in {
    val data = testOptionMetaData(ShortKey, HelpText)

    val generator = HelpGenerator.parameterNameColumnGenerator()
    generator(data) should contain only (HelpGenerator.DefaultShortOptionPrefix + ShortKey.key)
  }

  it should "support customizing the long prefix for the parameter name column generator" in {
    val Prefix = "/"
    val data = testOptionMetaData(Key, HelpText)

    val generator = HelpGenerator.parameterNameColumnGenerator(Prefix)
    generator(data) should contain only (Prefix + Key.key)
  }

  it should "support customizing the short prefix for the parameter name column generator" in {
    val Prefix = "#"
    val data = testOptionMetaData(ShortKey, HelpText)

    val generator = HelpGenerator.parameterNameColumnGenerator(shortOptionPrefix = Prefix)
    generator(data) should contain only (Prefix + ShortKey.key)
  }

  it should "handle keys without prefix in the parameter name column generator" in {
    val InputKey = ParameterKey("someInputParam", shortAlias = false, hasPrefix = false)
    val data = testOptionMetaData(InputKey, HelpText)

    val generator = HelpGenerator.parameterNameColumnGenerator()
    generator(data) should contain only InputKey.key
  }

  it should "provide a ColumnGenerator that renders the aliases of a parameter" in {
    val LongAlias = ParameterKey("alternative", shortAlias = false)
    val aliases = List(ShortKey, LongAlias)
    val expResult = List(HelpGenerator.DefaultShortOptionPrefix + ShortKey.key,
      HelpGenerator.DefaultLongOptionPrefix + LongAlias.key)
    val data = testOptionMetaData(Key, HelpText, aliases)

    val generator = HelpGenerator.parameterAliasColumnGenerator()
    generator(data) should contain theSameElementsInOrderAs expResult
  }

  it should "support customizing the prefixes for the alias column generator" in {
    val ShortPrefix = "\\"
    val LongPrefix = "/"
    val LongAlias = ParameterKey("alternative", shortAlias = false)
    val aliases = List(ShortKey, LongAlias)
    val expResult = List(ShortPrefix + ShortKey.key, LongPrefix + LongAlias.key)
    val data = testOptionMetaData(Key, HelpText, aliases)

    val generator = HelpGenerator.parameterAliasColumnGenerator(shortOptionPrefix = ShortPrefix,
      longOptionPrefix = LongPrefix)
    generator(data) should contain theSameElementsInOrderAs expResult
  }

  it should "provide a ColumnGenerator for the multiplicity of command line parameters" in {
    val Multiplicity = "many"
    val attributes = Map(ParameterModel.AttrMultiplicity -> Multiplicity)
    val data = ParameterMetaData(Key, ParameterAttributes(attributes))

    val generator = HelpGenerator.multiplicityColumnGenerator
    generator(data) should contain only Multiplicity
  }

  it should "provide a multiplicity ColumnGenerator that uses the correct default multiplicity" in {
    val data = testOptionMetaData(1)

    val generator = HelpGenerator.multiplicityColumnGenerator
    generator(data) should contain only Multiplicity.UnspecifiedMultiplicityString
  }

  it should "provide a ColumnGenerator for keys and aliases that handles missing aliases" in {
    val data = testOptionMetaData(Key, HelpText)
    val expResult = HelpGenerator.DefaultLongOptionPrefix + Key.key

    val generator = HelpGenerator.parameterKeyWithAliasesColumnGenerator()
    generator(data) should contain only expResult
  }

  it should "provide a ColumnGenerator for keys and aliases that displays aliases" in {
    val LongAlias = ParameterKey("anotherAlias", shortAlias = false)
    val aliases = List(ShortKey, LongAlias)
    val expResult = HelpGenerator.DefaultLongOptionPrefix + Key.key + ", " +
      HelpGenerator.DefaultShortOptionPrefix + ShortKey.key + ", " +
      HelpGenerator.DefaultLongOptionPrefix + LongAlias.key
    val data = testOptionMetaData(Key, HelpText, aliases)

    val generator = HelpGenerator.parameterKeyWithAliasesColumnGenerator()
    generator(data) should contain only expResult
  }

  it should "provide a ColumnGenerator for keys and aliases that supports customizing the separator" in {
    val Separator = ";"
    val LongAlias = ParameterKey("anotherAlias", shortAlias = false)
    val aliases = List(ShortKey, LongAlias)
    val expResult = HelpGenerator.DefaultLongOptionPrefix + Key.key + Separator +
      HelpGenerator.DefaultShortOptionPrefix + ShortKey.key + Separator +
      HelpGenerator.DefaultLongOptionPrefix + LongAlias.key
    val data = testOptionMetaData(Key, HelpText, aliases)

    val generator = HelpGenerator.parameterKeyWithAliasesColumnGenerator(separator = Separator)
    generator(data) should contain only expResult
  }

  it should "provide a ColumnGenerator for keys and aliases that supports customizing prefixes for aliases" in {
    val LongPrefix = "*"
    val ShortPrefix = "/"
    val LongAlias = ParameterKey("anotherLongAlias", shortAlias = false)
    val aliases = List(ShortKey, LongAlias)
    val expResult = LongPrefix + Key.key + ", " + ShortPrefix + ShortKey.key + ", " + LongPrefix + LongAlias.key
    val data = testOptionMetaData(Key, HelpText, aliases)

    val generator = HelpGenerator.parameterKeyWithAliasesColumnGenerator(longOptionPrefix = LongPrefix,
      shortOptionPrefix = ShortPrefix)
    generator(data) should contain only expResult
  }

  it should "provide a ColumnGenerator for keys and aliases that supports customizing prefixes for keys" in {
    val ShortPrefix = "_"
    val data = testOptionMetaData(ShortKey, HelpText)
    val expResult = ShortPrefix + ShortKey.key

    val generator = HelpGenerator.parameterKeyWithAliasesColumnGenerator(shortOptionPrefix = ShortPrefix)
    generator(data) should contain only expResult
  }

  it should "provide a ColumnGenerator for keys and alias that supports line wrapping" in {
    val LongAlias = ParameterKey("foo", shortAlias = false)
    val aliases = List(ShortKey, LongAlias)
    val expResult = List(HelpGenerator.DefaultLongOptionPrefix + Key.key + ",",
      HelpGenerator.DefaultShortOptionPrefix + ShortKey.key + ", " +
        HelpGenerator.DefaultLongOptionPrefix + LongAlias.key)
    val data = testOptionMetaData(Key, HelpText, aliases)

    val generator = HelpGenerator.parameterKeyWithAliasesColumnGenerator(maxLength = 13)
    generator(data) should be(expResult)
  }

  it should "provide a ColumnGenerator for a generated key and its aliases" in {
    val keyGenerator =
      HelpGenerator.mapLinesColumnGenerator(HelpGenerator.parameterNameColumnGenerator())("'" + _ + "'")
    val aliases = List(ShortKey)
    val expResult = List("'" + HelpGenerator.DefaultLongOptionPrefix + Key.key + "', " +
      HelpGenerator.DefaultShortOptionPrefix + ShortKey.key)
    val data = testOptionMetaData(Key, HelpText, aliases)

    val generator = HelpGenerator.parameterKeyGeneratedWithAliasesColumnGenerator(keyGenerator)
    generator(data) should be(expResult)
  }

  it should "provide a ColumnGenerator that detects a mandatory parameter" in {
    val Output = "It's mandatory"
    val attributes = Map(ParameterModel.AttrMultiplicity -> "1..*")
    val data = ParameterMetaData(Key, ParameterAttributes(attributes))

    val generator = HelpGenerator.mandatoryColumnGenerator(optMandatoryText = Some(Output),
      optOptionalText = Some("This text should not appear"))
    generator(data) should contain only Output
  }

  it should "provide a ColumnGenerator that detects a parameter with default value as optional" in {
    val Output = "No"
    val attributes = Map(ParameterModel.AttrMultiplicity -> "1..1",
      ParameterModel.AttrFallbackValue -> "fallback")
    val data = ParameterMetaData(Key, ParameterAttributes(attributes))

    val generator = HelpGenerator.mandatoryColumnGenerator(optMandatoryText = Some("yes"),
      optOptionalText = Some(Output))
    generator(data) should contain only Output
  }

  it should "provide a ColumnGenerator that detects an optional parameter" in {
    val Output = "It's optional"
    val attributes = Map(ParameterModel.AttrMultiplicity -> "0..1")
    val data = ParameterMetaData(Key, ParameterAttributes(attributes))

    val generator = HelpGenerator.mandatoryColumnGenerator(optMandatoryText = Some("to ignore"),
      optOptionalText = Some(Output))
    generator(data) should contain only Output
  }

  it should "provide a ColumnGenerator detecting mandatory parameters that handles empty texts" in {
    val attributes = Map.empty[String, String]
    val data = ParameterMetaData(Key, ParameterAttributes(attributes))

    val generator = HelpGenerator.mandatoryColumnGenerator()
    generator(data) should have size 0
  }
}
