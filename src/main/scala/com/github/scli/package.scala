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

package com.github

import java.util.Locale

package object scli {
  /**
   * Converts a string to lower case.
   *
   * @param s the string
   * @return the string in lowercase
   */
  def toLowerCase(s: String): String = s.toLowerCase(Locale.ROOT)

  /**
   * Converts a string to uppercase.
   *
   * @param s the string
   * @return the string in uppercase
   */
  def toUpperCase(s: String): String = s.toUpperCase(Locale.ROOT)
}
