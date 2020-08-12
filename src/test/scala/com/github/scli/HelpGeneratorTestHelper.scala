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

import com.github.scli.ParameterModel.{ParameterAttributes, ParameterKey, ParameterMetaData}

/**
 * An object defining some commons constants and functions used by tests of the
 * help generator.
 */
object HelpGeneratorTestHelper {
  /** Key for a test option. */
  final val Key = ParameterKey("testOption", shortAlias = false)

  /** A key representing the short alias of a test option. */
  final val ShortKey = ParameterKey("o", shortAlias = true)

  /** A test help text. */
  final val HelpText = "Test help text for the test help option."

  /**
   * Generates the key of the test option with the given index.
   *
   * @param idx the index of the test option
   * @return the key for this option
   */
  def testKey(idx: Int): ParameterKey = ParameterKey(s"$Key$idx", shortAlias = false)

  /**
   * Generates test meta data for an option.
   *
   * @param idx the index of the test option
   * @return the test meta data
   */
  def testOptionMetaData(idx: Int): ParameterMetaData = {
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
  def testOptionMetaData(key: ParameterKey, help: String): ParameterMetaData = {
    val attrs = Map(ParameterModel.AttrHelpText -> help,
      ParameterModel.AttrParameterType -> ParameterModel.ParameterTypeOption)
    ParameterMetaData(key, ParameterAttributes(attrs))
  }

}
