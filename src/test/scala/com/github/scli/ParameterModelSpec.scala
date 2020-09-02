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

import com.github.scli.ParameterModel.{ParameterAttributeKey, ParameterAttributes}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

object ParameterModelSpec {
  /** Test key of a string attribute. */
  private val AttrString = ParameterAttributeKey[String]("strAttr")

  /** Test key of a numeric attribute. */
  private val AttrInt = ParameterAttributeKey[Integer]("intAttr")

  /** Test value for the string attribute. */
  private val StrValue = "this is a test string value"

  /** Test value for the numeric attribute. */
  private val NumValue: Integer = 77
}

/**
 * Test class for classes related to ''ParameterModel''.
 */
class ParameterModelSpec extends AnyFlatSpec with Matchers {

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
}
