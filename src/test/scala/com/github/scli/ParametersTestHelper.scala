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

import com.github.scli.ParameterExtractor.Parameters
import com.github.scli.ParameterModel.ParameterKey
import com.github.scli.ParameterParser.{OptionElement, ParametersMap}

import scala.language.implicitConversions

/**
 * A helper module providing conversion functions that simplify the declaration
 * of test parameters for the execution of extractors.
 *
 * This object offers some functions and implicit conversions that simplify the
 * creation of ''ParameterKey'' objects. This is needed by many test classes.
 */
object ParametersTestHelper {
  /**
   * Implicit conversion function to simplify the declaration of ''Parameters''
   * objects from maps.
   *
   * @param p the string-based map with parameters
   * @return the ''Parameters'' instance
   */
  implicit def toParameters(p: Map[String, Iterable[String]]): Parameters =
    Parameters(toParametersMap(p), Set.empty)

  /**
   * Implicit conversion function to simplify the declaration of maps with
   * parameters.
   *
   * @param p the original string-based map
   * @return the map with parameters
   */
  implicit def toParametersMap(p: Map[String, Iterable[String]]): ParametersMap =
    p map { t =>
      val key = ParameterKey(t._1, shortAlias = false)
      val values = t._2 map (s => OptionElement(key, Some(s)))
      (key, values)
    }

  /**
   * Converts a map with string parameter values to a ''ParametersMap''.
   *
   * @param map the map with string values
   * @return the ''ParametersMap''
   */
  def toParamValues(map: Map[ParameterKey, Iterable[String]]): ParametersMap =
    map map { e =>
      val values = e._2 map (s => OptionElement(e._1, Some(s)))
      (e._1, values)
    }

  /**
   * Convenience function to create a ''ParameterKey'' from a string with
   * default settings.
   *
   * @param key the string-based key
   * @return the resulting ''ParameterKey''
   */
  def pk(key: String): ParameterKey =
    ParameterKey(key, shortAlias = false)

  /**
   * Converts a set with strings to a set of ''ParameterKey'' objects.
   *
   * @param set the original set with strings
   * @return the set with ''ParameterKey'' objects
   */
  def pkSet(set: Set[String]): Set[ParameterKey] =
    set map pk

  /**
   * Convenience function to construct a ''Parameters'' instance from the given
   * string-based map and a string-based set of accessed parameters.
   *
   * @param pMap     the string-based map
   * @param accessed the string-based set
   * @return the ''Parameters'' instance
   */
  def toParametersWithAccessed(pMap: Map[String, Iterable[String]], accessed: Set[String]): Parameters =
    Parameters(toParametersMap(pMap), pkSet(accessed))
}
