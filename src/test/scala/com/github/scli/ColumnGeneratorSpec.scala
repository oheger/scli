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

import com.github.scli.CliHelpGenerator.ColumnGenerator
import com.github.scli.ParameterModel.{ParameterAttributes, ParameterKey, ParameterMetaData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Test class for the several column generators provided by the help generator
 * module.
 */
class ColumnGeneratorSpec extends AnyFlatSpec with Matchers {

  import HelpGeneratorTestHelper._

  "CliHelpGenerator" should "provide an attribute ColumnGenerator that handles undefined attributes" in {
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

  it should "provide a ColumnGenerator that composes a line from two generators" in {
    val attributes = Map("a1" -> "v1", "a2" -> "v2")
    val data = ParameterMetaData(Key, ParameterAttributes(attributes))
    val g1 = CliHelpGenerator.attributeColumnGenerator("a1")
    val g2 = CliHelpGenerator.attributeColumnGenerator("a2")

    val generator = CliHelpGenerator.composeSingleLineColumnGenerator(g1, g2, ", ")
    val result = generator(data)
    result should be(List("v1, v2"))
  }

  it should "provide a ColumnGenerator that composes a line if generator 2 does not produce a result" in {
    val attributes = Map("a1" -> "v1")
    val data = ParameterMetaData(Key, ParameterAttributes(attributes))
    val g1 = CliHelpGenerator.attributeColumnGenerator("a1")
    val g2 = CliHelpGenerator.attributeColumnGenerator("a2")

    val generator = CliHelpGenerator.composeSingleLineColumnGenerator(g1, g2, ", ")
    val result = generator(data)
    result should be(List("v1"))
  }

  it should "provide a ColumnGenerator that composes a line if generator 1 does not produce a result" in {
    val attributes = Map("a1" -> "v1")
    val data = ParameterMetaData(Key, ParameterAttributes(attributes))
    val g1 = CliHelpGenerator.attributeColumnGenerator("a1")
    val g2 = CliHelpGenerator.attributeColumnGenerator("a2")

    val generator = CliHelpGenerator.composeSingleLineColumnGenerator(g2, g1, ", ")
    val result = generator(data)
    result should be(List("v1"))
  }

  it should "provide a ColumnGenerator that composes a line if both generators do not produce a result" in {
    val data = ParameterMetaData(Key, ParameterAttributes(Map.empty))
    val g1 = CliHelpGenerator.attributeColumnGenerator("a1")
    val g2 = CliHelpGenerator.attributeColumnGenerator("a2")

    val generator = CliHelpGenerator.composeSingleLineColumnGenerator(g2, g1, "*")
    val result = generator(data)
    result should have size 0
  }

  it should "provide a ColumnGenerator that appends a separator and handles an empty result" in {
    val data = ParameterMetaData(Key, ParameterAttributes(Map.empty))
    val gen = CliHelpGenerator.attributeColumnGenerator("someKey")

    val generator = CliHelpGenerator.separatorColumnGenerator(gen, "---")
    val result = generator(data)
    result should have size 0
  }

  it should "provide a ColumnGenerator that appends a separator to a single-line result" in {
    val Attr = "testAttr"
    val Value = "aValue"
    val data = ParameterMetaData(Key, ParameterAttributes(Map(Attr -> Value)))
    val gen = CliHelpGenerator.attributeColumnGenerator(Attr)

    val generator = CliHelpGenerator.separatorColumnGenerator(gen, "***")
    val result = generator(data)
    result should contain only Value
  }

  it should "provide a ColumnGenerator that appends a separator to a multi-line result" in {
    val Attr = "multiLineAttr"
    val data = ParameterMetaData(Key, ParameterAttributes(Map(Attr -> "v1,v2,v3")))
    val gen: ColumnGenerator = data => data.attributes.attributes(Attr).split(",").toList

    val generator = CliHelpGenerator.separatorColumnGenerator(gen, ";")
    val result = generator(data)
    result should be(List("v1;", "v2;", "v3"))
  }

  it should "provide a ColumnGenerator that converts a result to a single line" in {
    val Attr = "multiLineAttr"
    val data = ParameterMetaData(Key, ParameterAttributes(Map(Attr -> "v1,v2,v3")))
    val gen: ColumnGenerator = data => data.attributes.attributes(Attr).split(",").toList

    val generator = CliHelpGenerator.singleLineColumnGenerator(gen, "; ")
    val result = generator(data)
    result should contain only "v1; v2; v3"
  }

  it should "provide a ColumnGenerator that converts a result to a single line and handles an empty result" in {
    val data = ParameterMetaData(Key, ParameterAttributes(Map.empty))
    val gen = CliHelpGenerator.attributeColumnGenerator("undefined")

    val generator = CliHelpGenerator.singleLineColumnGenerator(gen, "~~~")
    val result = generator(data)
    result should have size 0
  }

  it should "provide a ColumnGenerator that converts a result to a single line and handles a single line" in {
    val Attr = "singleLineAttr"
    val Value = "foo"
    val data = ParameterMetaData(Key, ParameterAttributes(Map(Attr -> Value)))
    val gen = CliHelpGenerator.attributeColumnGenerator(Attr)

    val generator = CliHelpGenerator.singleLineColumnGenerator(gen, "===")
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
    generator(data) should contain only (CliHelpGenerator.DefaultLongOptionPrefix + Key.key)
  }

  it should "provide a ColumnGenerator that outputs the correct prefix for the parameter name" in {
    val data = testOptionMetaData(ShortKey, HelpText)

    val generator = CliHelpGenerator.parameterNameColumnGenerator()
    generator(data) should contain only (CliHelpGenerator.DefaultShortOptionPrefix + ShortKey.key)
  }

  it should "support customizing the long prefix for the parameter name column generator" in {
    val Prefix = "/"
    val data = testOptionMetaData(Key, HelpText)

    val generator = CliHelpGenerator.parameterNameColumnGenerator(Prefix)
    generator(data) should contain only (Prefix + Key.key)
  }

  it should "support customizing the short prefix for the parameter name column generator" in {
    val Prefix = "#"
    val data = testOptionMetaData(ShortKey, HelpText)

    val generator = CliHelpGenerator.parameterNameColumnGenerator(shortOptionPrefix = Prefix)
    generator(data) should contain only (Prefix + ShortKey.key)
  }

  it should "provide a ColumnGenerator that renders the aliases of a parameter" in {
    val LongAlias = ParameterKey("alternative", shortAlias = false)
    val aliases = List(ShortKey, LongAlias)
    val expResult = List(CliHelpGenerator.DefaultShortOptionPrefix + ShortKey.key,
      CliHelpGenerator.DefaultLongOptionPrefix + LongAlias.key)
    val data = testOptionMetaData(Key, HelpText, aliases)

    val generator = CliHelpGenerator.parameterAliasColumnGenerator()
    generator(data) should contain theSameElementsInOrderAs expResult
  }

  it should "support customizing the prefixes for the alias column generator" in {
    val ShortPrefix = "\\"
    val LongPrefix = "/"
    val LongAlias = ParameterKey("alternative", shortAlias = false)
    val aliases = List(ShortKey, LongAlias)
    val expResult = List(ShortPrefix + ShortKey.key, LongPrefix + LongAlias.key)
    val data = testOptionMetaData(Key, HelpText, aliases)

    val generator = CliHelpGenerator.parameterAliasColumnGenerator(shortOptionPrefix = ShortPrefix,
      longOptionPrefix = LongPrefix)
    generator(data) should contain theSameElementsInOrderAs expResult
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

  it should "provide a ColumnGenerator for keys and aliases that handles missing aliases" in {
    val data = testOptionMetaData(Key, HelpText)
    val expResult = CliHelpGenerator.DefaultLongOptionPrefix + Key.key

    val generator = CliHelpGenerator.parameterKeyWithAliasesColumnGenerator()
    generator(data) should contain only expResult
  }

  it should "provide a ColumnGenerator for keys and aliases that displays aliases" in {
    val LongAlias = ParameterKey("anotherAlias", shortAlias = false)
    val aliases = List(ShortKey, LongAlias)
    val expResult = CliHelpGenerator.DefaultLongOptionPrefix + Key.key + ", " +
      CliHelpGenerator.DefaultShortOptionPrefix + ShortKey.key + ", " +
      CliHelpGenerator.DefaultLongOptionPrefix + LongAlias.key
    val data = testOptionMetaData(Key, HelpText, aliases)

    val generator = CliHelpGenerator.parameterKeyWithAliasesColumnGenerator()
    generator(data) should contain only expResult
  }

  it should "provide a ColumnGenerator for keys and aliases that supports customizing the separator" in {
    val Separator = ";"
    val LongAlias = ParameterKey("anotherAlias", shortAlias = false)
    val aliases = List(ShortKey, LongAlias)
    val expResult = CliHelpGenerator.DefaultLongOptionPrefix + Key.key + Separator +
      CliHelpGenerator.DefaultShortOptionPrefix + ShortKey.key + Separator +
      CliHelpGenerator.DefaultLongOptionPrefix + LongAlias.key
    val data = testOptionMetaData(Key, HelpText, aliases)

    val generator = CliHelpGenerator.parameterKeyWithAliasesColumnGenerator(separator = Separator)
    generator(data) should contain only expResult
  }

  it should "provide a ColumnGenerator for keys and aliases that supports customizing prefixes for aliases" in {
    val LongPrefix = "*"
    val ShortPrefix = "/"
    val LongAlias = ParameterKey("anotherLongAlias", shortAlias = false)
    val aliases = List(ShortKey, LongAlias)
    val expResult = LongPrefix + Key.key + ", " + ShortPrefix + ShortKey.key + ", " + LongPrefix + LongAlias.key
    val data = testOptionMetaData(Key, HelpText, aliases)

    val generator = CliHelpGenerator.parameterKeyWithAliasesColumnGenerator(longOptionPrefix = LongPrefix,
      shortOptionPrefix = ShortPrefix)
    generator(data) should contain only expResult
  }

  it should "provide a ColumnGenerator for keys and aliases that supports customizing prefixes for keys" in {
    val ShortPrefix = "_"
    val data = testOptionMetaData(ShortKey, HelpText)
    val expResult = ShortPrefix + ShortKey.key

    val generator = CliHelpGenerator.parameterKeyWithAliasesColumnGenerator(shortOptionPrefix = ShortPrefix)
    generator(data) should contain only expResult
  }

  it should "provide a ColumnGenerator for keys and alias that supports line wrapping" in {
    val LongAlias = ParameterKey("foo", shortAlias = false)
    val aliases = List(ShortKey, LongAlias)
    val expResult = List(CliHelpGenerator.DefaultLongOptionPrefix + Key.key + ",",
      CliHelpGenerator.DefaultShortOptionPrefix + ShortKey.key + ", " +
        CliHelpGenerator.DefaultLongOptionPrefix + LongAlias.key)
    val data = testOptionMetaData(Key, HelpText, aliases)

    val generator = CliHelpGenerator.parameterKeyWithAliasesColumnGenerator(maxLength = 13)
    generator(data) should be(expResult)
  }
}
