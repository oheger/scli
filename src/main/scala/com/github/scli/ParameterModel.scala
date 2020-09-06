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

import scala.collection.SortedSet
import scala.reflect.ClassTag

/**
 * A module providing classes and constants to construct a model for the
 * parameters supported by an application.
 *
 * The model is generated based on the extractors declared by the application.
 * This information is then used for different purposes, for instance to parse
 * the command line correctly or to generate help information.
 *
 * The parameter model mainly consists of a context object storing information
 * about all parameters defined. For each parameter, its key, its type, and a
 * map with attributes are stored. The attributes are populated automatically
 * by specific extractor functions.
 */
object ParameterModel {
  /** The attribute representing the help text of an option. */
  final val AttrHelpText = ParameterAttributeKey[String]("helpText")

  /** The attribute with a description for the fallback value. */
  final val AttrFallbackValue = ParameterAttributeKey[String]("fallbackValue")

  /** The attribute defining the multiplicity of an option. */
  final val AttrMultiplicity = ParameterAttributeKey[Multiplicity]("multiplicity")

  /**
   * The attribute assigning a group to an option. Groups are used to handle
   * options that are valid only in certain constellations, e.g. when
   * conditional extractors are involved, or if a CLI supports multiple
   * commands, each of which has its own set of options. In the help text it
   * can then be indicated that the options belonging to a group are allowed
   * only if specific conditions are fulfilled.
   */
  final val AttrGroup = ParameterAttributeKey[Set[String]]("group")

  /**
   * The attribute defining the type of a parameter. This attribute contains
   * the information whether a parameter is an option, a switch, or an input
   * parameter.
   */
  final val AttrParameterType = ParameterAttributeKey[String]("parameterType")

  /**
   * The attribute to store an error message for an option. The attribute can
   * be set if parameter extraction has failed for an option. It is then
   * possible to generate output containing all error messages using all the
   * column generators supported.
   */
  final val AttrErrorMessage = ParameterAttributeKey[String]("errorMessage")

  /**
   * The attribute defining the value to be assigned to a switch if it is
   * present. This is normally a boolean constant.
   */
  final val AttrSwitchValue = ParameterAttributeKey[String]("switchValue")

  /** Parameter type indicating an option. */
  final val ParameterTypeOption = "option"

  /** Parameter type indicating a command line switch. */
  final val ParameterTypeSwitch = "switch"

  /** Parameter type indicating an input parameter. */
  final val ParameterTypeInput = "input"

  /**
   * A prefix for keys for input parameters that are generated. This is used
   * if for an input parameter no key has been provided explicitly. The index
   * of the input parameter is appended.
   */
  final val KeyInput = "input"

  /** Constant for an initial, empty mapping for parameter aliases. */
  final val EmptyAliasMapping = AliasMapping(Map.empty, Map.empty)

  /**
   * Constant for a special group name referencing an unassigned group. It can
   * happen that extractors are executed inside and outside a group context.
   * Using this special group name, it can be determined whether the extractor
   * has been used at least once outside any group context.
   */
  private[scli] val UnassignedGroup = "scli:unassigned"

  /** A special set containing only the unassigned group. */
  private val UnassignedGroupSet = Set(UnassignedGroup)

  /**
   * A data class to represent the key of a parameter.
   *
   * Some CLI applications distinguish between normal (long) parameter names
   * and short alias names. On the command line, typically a different prefix
   * is used to indicate a normal name or its alias (e.g. "--" for the normal
   * names and "-" for short names).
   *
   * To support such constellations, a plain string as parameter key is
   * insufficient. Therefore, this class is used to represent the keys of
   * parameters. It makes it explicit whether a string is to be interpreted as
   * normal or short key.
   *
   * There are even keys that do not have a prefix at all: keys for input
   * parameters. Such keys can be modeled using this class as well.
   *
   * @param key        the string-based key
   * @param shortAlias flag whether this key is a short alias
   * @param hasPrefix  flag whether this key requires a prefix
   */
  case class ParameterKey(key: String,
                          shortAlias: Boolean,
                          hasPrefix: Boolean = true)

  /**
   * A data class for keeping track on alias names assigned to parameters.
   *
   * This class allows resolving alias names for parameter keys and the
   * parameter key for a specific alias. It is stored in the [[ModelContext]]
   * to handle aliases effectively.
   *
   * @param aliasesForKey stores the aliases assigned to a parameter
   * @param keyForAlias   reverse mapping from an alias to the key
   */
  case class AliasMapping(aliasesForKey: Map[ParameterKey, List[ParameterKey]],
                          keyForAlias: Map[ParameterKey, ParameterKey]) {
    /**
     * Returns a new ''AliasMapping'' instance with the new alias provided.
     *
     * @param key   the key to which an alias should be added
     * @param alias the new alias
     * @return the updated ''AliasMapping''
     */
    def addAlias(key: ParameterKey, alias: ParameterKey): AliasMapping = {
      val aliasList = aliasesForKey.getOrElse(key, Nil)
      if (aliasList.contains(alias)) this
      else {
        val updatedAliasList = aliasList :+ alias
        AliasMapping(aliasesForKey + (key -> updatedAliasList),
          keyForAlias + (alias -> key))
      }
    }
  }

  object ParameterAttributeKey {
    /**
     * Creates a new ''ParameterAttributeKey'' instance with a specific key.
     * This is a convenience function that automatically obtains the class for
     * the attribute's data type.
     *
     * @param key the actual key of the attribute
     * @param ct  the class tag
     * @tparam A the data type of the attribute
     * @return the newly created ''ParameterAttributeKey''
     */
    def apply[A <: AnyRef](key: String)(implicit ct: ClassTag[A]): ParameterAttributeKey[A] = {
      val attrType = ct.runtimeClass.asInstanceOf[Class[A]]
      new ParameterAttributeKey(key, attrType)
    }
  }

  /**
   * A data class representing the key of a metadata attribute associated with
   * a parameter.
   *
   * Attributes have a key and a data type. The information stored in an
   * instance allows type-safe access to the values of such attributes.
   *
   * @param key   the key of this attribute
   * @param clazz the class representing the attribute's data type
   * @tparam A the type of the attribute
   */
  case class ParameterAttributeKey[A <: AnyRef](key: String, clazz: Class[A])

  /**
   * A data class storing information about a single command line parameter.
   *
   * An instance of this class contains metadata about a parameter that can be
   * used for various purposes, e.g. to generate the help text of a CLI
   * application. Some of the attributes are also used to implement specific
   * features during command line processing. The metadata is extensible and
   * is therefore stored as a map of attributes. Type-safe access to attribute
   * values is possible by using typed keys.
   *
   * @param attributes stores the map with attributes
   */
  case class ParameterAttributes private(attributes: Map[ParameterAttributeKey[_], AnyRef]) {
    def this() = this(Map.empty)

    /**
     * Returns a new instance with all attributes of this object plus the
     * mapping specified.
     *
     * @param key   the key of the attribute to add
     * @param value the value of the new attribute
     * @tparam A the data type of the attribute
     * @return the updated ''ParameterAttributes'' instance
     */
    def add[A <: AnyRef](key: ParameterAttributeKey[A], value: A): ParameterAttributes =
      this.+(key -> value)

    /**
     * Alias for ''add()''.
     *
     * @param mapping the mapping to be added to the attributes
     * @tparam A the data type of the attribute
     * @return the updated ''ParameterAttributes'' instance
     */
    def +[A <: AnyRef](mapping: (ParameterAttributeKey[A], A)): ParameterAttributes = {
      val map = attributes + mapping
      ParameterAttributes(map)
    }

    /**
     * Queries the attribute specified by the given key and returns an
     * ''Option'' with the result.
     *
     * @param key the key of the desired attribute
     * @tparam A the data type of the attribute
     * @return an ''Option'' with the attribute value; ''None'' if the
     *         attribute is not present
     */
    def get[A <: AnyRef](key: ParameterAttributeKey[A]): Option[A] =
      attributes.get(key).map(key.clazz.cast)

    /**
     * Queries the attribute specified by the given key and returns a default
     * value if the attribute is not present.
     *
     * @param key     the key of the desired attribute
     * @param default the default value of this attribute
     * @tparam A the data type of the attribute
     * @tparam B the data type of the default value
     * @return the attribute value or the default value
     */
    def getOrElse[A <: AnyRef, B <: A](key: ParameterAttributeKey[A], default: => B): A =
      get(key) getOrElse default

    /**
     * Tests whether this object contains an attribute with the given key.
     *
     * @param key the key in question
     * @return a flag whether an attribute with this key exists
     */
    def contains(key: ParameterAttributeKey[_]): Boolean =
      attributes contains key

    /**
     * Returns a ''ParameterAttributes'' instance that contains the union of
     * the attributes of this instance and the specified instance. Attributes
     * in the other instance take precedence.
     *
     * @param other the instance to add
     * @return a ''ParameterAttributes'' instance with the union of attributes
     */
    def addAll(other: ParameterAttributes): ParameterAttributes =
      ParameterAttributes(attributes ++ other.attributes)

    /**
     * Alias for ''addAll()''.
     *
     * @param other the instance to add
     * @return a ''ParameterAttributes'' instance with the union of attributes
     */
    def ++(other: ParameterAttributes): ParameterAttributes = addAll(other)
  }

  /**
   * A data class holding information about input parameters.
   *
   * Input parameters are values passed to an application directly and not as
   * options. They can be assigned a key (a short name) and a detailed help
   * text with a description. The attributes of input parameters are stored in
   * the same way as for command line options. This class is used to hold an
   * ordered set of references to input parameters (as the order of these
   * parameters is typically relevant) from their indices to their keys. From
   * the keys, the other information available can be retrieved.
   *
   * @param index the index of the input parameter
   * @param key   the key of the input parameter
   */
  case class InputParameterRef(index: Int, key: ParameterKey) extends Ordered[InputParameterRef] {
    /**
     * @inheritdoc This implementation orders input parameter references by
     *             their index, treating negative indices in a special way, so
     *             that they appear after the positive ones. (Negative indices
     *             reference the last input parameters.)
     */
    override def compare(that: InputParameterRef): Int = {
      val sigThis = sig(index)
      val sigThat = sig(that.index)
      if (sigThis != sigThat) sigThat
      else index - that.index
    }
  }

  /**
   * A data class containing all the information available for a CLI parameter.
   *
   * @param key        the key of the parameter
   * @param attributes a data object with the attributes of this option
   * @param aliases    a list with aliases defined for this parameter
   */
  case class ParameterMetaData(key: ParameterKey,
                               attributes: ParameterAttributes,
                               aliases: List[ParameterKey] = Nil)

  /**
   * A trait providing access to metadata for parameters.
   *
   * This trait is extended by different context objects that store parameters
   * and metadata attributes about them. For all of these context
   * implementations it is then possible to iterate of the parameters and their
   * data in a uniform way.
   */
  trait ParameterMetaDataSource {
    /**
     * Returns an ''Iterable'' with the ''ParameterMetaData'' stored in this
     * object.
     *
     * @return an ''Iterable'' with the ''ParameterMetaData'' available
     */
    def parameterMetaData: Iterable[ParameterMetaData]
  }

  /**
   * A class for storing and updating meta information about command line
   * parameters.
   *
   * An instance of this class is available in the context passed to
   * ''CliExtractor'' objects. The extractors update the instance with specific
   * information, so that metadata about the options supported by the
   * application is collected. This data can then be used for various purposes,
   * e.g. to correctly parse the command line or to generate rich help texts.
   *
   * @param options       a map storing the data available for the single options
   * @param inputs        a set with data about input parameters
   * @param aliasMapping  the object with information about aliases
   * @param optCurrentKey a key to the option that is currently defined
   * @param groups        a list with the currently active group names
   */
  class ModelContext(val options: Map[ParameterKey, ParameterAttributes],
                     val inputs: SortedSet[InputParameterRef],
                     val aliasMapping: AliasMapping,
                     optCurrentKey: Option[ParameterKey],
                     groups: List[String]) extends ParameterMetaDataSource {

    /**
     * Adds data about another command line option to this object. This
     * function creates a new [[ParameterAttributes]] instance and initializes it
     * from the parameters passed in. It returns a new ''ModelContext'' object
     * whose data map contains this new instance. If there is already an entry
     * for this parameter key, it is merged with the data passed to this
     * function.
     *
     * @param key  the parameter key
     * @param text an optional help text
     * @return the updated ''ModelContext''
     */
    def addOption(key: ParameterKey, text: Option[String]): ModelContext =
      contextWithParameter(key, text, ParameterTypeOption, inputs)

    /**
     * Adds data about an input parameter to this object. This function works
     * similar to ''addOption()'', but creates additional information to keep
     * track on the order of these parameters.
     *
     * @param index  the index of the input parameter
     * @param optKey the optional key; if it is undefined, a key is generated
     * @param text   an optional help text
     * @return the updated ''ModelContext''
     */
    def addInputParameter(index: Int, optKey: Option[String], text: Option[String]): ModelContext = {
      val key = optKey.getOrElse(KeyInput + index)
      val paramKey = ParameterKey(key, shortAlias = false, hasPrefix = false)
      val inputRef = InputParameterRef(index, paramKey)
      contextWithParameter(paramKey, text, ParameterTypeInput,
        inputs union Set(inputRef))
    }

    /**
     * Adds an attribute for the current option. This function is called by
     * ''CliExtractor'' objects to add more detailed information about a
     * command line option. It refers to the last option that has been added.
     *
     * @param attrKey the key of the attribute
     * @param value   the value of the attribute
     * @tparam A the data type of the attribute
     * @return the updated ''ModelContext''
     */
    def addAttribute[A <: AnyRef](attrKey: ParameterAttributeKey[A], value: A): ModelContext =
      updateWithCurrentKey { key =>
        val attrs = options(key)
        val newAttrs = attrs + (attrKey -> value)
        copy(options = options + (key -> newAttrs))
      }

    /**
     * Checks whether the command line option with the given key has a specific
     * attribute set. Only the presence of the attribute is checked, not the
     * concrete value.
     *
     * @param key     the key of the parameter
     * @param attrKey the key of the attribute
     * @return a flag whether this attribute is present for this option; if the
     *         option cannot be resolved, result is '''false'''
     */
    def hasAttribute(key: ParameterKey, attrKey: ParameterAttributeKey[_]): Boolean =
      options.get(key) exists (_ contains attrKey)

    /**
     * Notifies this context about the start of a new group. New options that
     * are added later are assigned to this group.
     *
     * @param groupName the name of the group
     * @return the updated ''ModelContext''
     */
    def startGroup(groupName: String): ModelContext =
      copy(groups = groupName :: groups)

    /**
     * Notifies this context about a potential start of a new group. If the
     * group name is defined, a new group is started; otherwise, the same
     * context is returned.
     *
     * @param optGroupName the optional group name
     * @return the updated ''ModelContext'' or the same one
     */
    def startGroupConditionally(optGroupName: Option[String]): ModelContext =
      optGroupName.fold(this)(startGroup)

    /**
     * Notifies this context that a group has been processed. The name of the
     * current group is removed.
     *
     * @return the updated ''ModelContext''
     */
    def endGroup(): ModelContext = copy(groups = groups.tail)

    /**
     * Notifies this context that a group has potentially been processed. If
     * the given ''Option'' with the group name is defined, the name of the
     * current group is removed; otherwise, this context is returned
     * unchanged.
     *
     * @param optGroupName the ''Option'' with the group name
     * @return the updated ''ModelContext'' or the same one
     */
    def endGroupConditionally(optGroupName: Option[String]): ModelContext =
      optGroupName.fold(this)(_ => endGroup())

    /**
     * Returns an updated ''ModelContext'' with a an [[AliasMapping]] that is
     * modified to contain the new alias provided for the current key.
     *
     * @param alias the new alias for this key
     * @return the updated ''ModelContext''
     */
    def addAlias(alias: ParameterKey): ModelContext =
      updateWithCurrentKey { key =>
        copy(aliasMapping = aliasMapping.addAlias(key, alias))
      }

    /**
     * @inheritdoc This implementation returns data about all the parameters
     *             defined by the application.
     */
    override def parameterMetaData: Iterable[ParameterMetaData] =
      options.map(e => ParameterMetaData(e._1, e._2,
        aliases = aliasMapping.aliasesForKey.getOrElse(e._1, Nil)))

    /**
     * Creates a new ''ModelContext'' with an additional parameter as defined
     * by the data provided.
     *
     * @param key        the parameter key
     * @param text       the help text for the option
     * @param optionType the type of the option
     * @param inputRefs  the input data for the new context
     * @return the updated ''ModelContext''
     */
    private def contextWithParameter(key: ParameterKey, text: Option[String], optionType: String,
                                     inputRefs: SortedSet[InputParameterRef]): ModelContext = {
      val existingAttrs = options.getOrElse(key, new ParameterAttributes)
      val existingGroups = ParameterModel.groups(existingAttrs)
      val attrs = addOptionalAttribute(new ParameterAttributes, AttrHelpText, text) +
        (AttrGroup -> (existingGroups ++ groupAttribute)) + (AttrParameterType -> optionType)
      val newAttrs = existingAttrs ++ attrs
      copy(options = options + (key -> newAttrs), inputs = inputRefs, optCurrentKey = Some(key))
    }

    /**
     * Executes a function to update this context that depends on the current
     * parameter key. If no current key is available, this context is returned
     * unchanged.
     *
     * @param f the update function
     * @return the updated model context
     */
    private def updateWithCurrentKey(f: ParameterKey => ModelContext): ModelContext =
      optCurrentKey.fold(this)(f)

    /**
     * Returns the current value of the group attribute. If there are active
     * groups, their names are returned as a set; otherwise, the resulting set
     * contains only the special ''unassigned'' group.
     *
     * @return the names of the currently active groups
     */
    private def groupAttribute: Set[String] =
      if (groups.isEmpty) UnassignedGroupSet
      else groups.toSet

    /**
     * Convenience function to create a copy of this object with some
     * attributes modified.
     *
     * @param options       the new map with options
     * @param inputs        the new set with input parameters
     * @param aliasMapping  the new alias mapping
     * @param optCurrentKey the new current key
     * @param groups        the new groups
     * @return the modified copy
     */
    private def copy(options: Map[ParameterKey, ParameterAttributes] = options,
                     inputs: SortedSet[InputParameterRef] = inputs,
                     aliasMapping: AliasMapping = aliasMapping,
                     optCurrentKey: Option[ParameterKey] = optCurrentKey,
                     groups: List[String] = groups): ModelContext =
      new ModelContext(options, inputs, aliasMapping, optCurrentKey, groups)
  }

  /**
   * A trait representing a failure during processing of a parameter.
   *
   * The failure can be related to a parameter as a whole, e.g. if too many or
   * too few values are provided. It can also refer to a single parameter
   * value; this is the case if a transformation on this value caused an
   * exception.
   */
  trait ParameterFailure {
    /**
     * Returns the key of the parameter affected by this failure.
     *
     * @return the parameter key
     */
    def key: ParameterKey

    /**
     * Returns the key of the parameter used when this failure occurred. This
     * can either be the main parameter key or one of its aliases.
     *
     * @return the key related to this failure
     */
    def failureKey: ParameterKey

    /**
     * Returns the exception that was the cause of this failure.
     *
     * @return the exception causing this failure
     */
    def cause: Throwable

    /**
     * Returns the original value as passed on the command line that caused
     * this failure if available. This ''Option'' is defined only if this
     * failure is related to a concrete value, and the value could be
     * unambiguously identified with one of the CLI elements.
     *
     * @return an ''Option'' with the original parameter value
     */
    def optOriginalValue: Option[String]
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
    attrs.get(AttrGroup) exists (_ contains group)

  /**
   * Returns a set with the names of all groups the option whose attributes
   * are provided belongs to.
   *
   * @param attrs the ''OptionAttributes''
   * @return a set with the names of all groups
   */
  def groups(attrs: ParameterAttributes): Set[String] =
    attrs.get(AttrGroup) getOrElse Set.empty

  /**
   * Adds an attribute and its value to the given map of attributes only if
   * the value is defined.
   *
   * @param attributes the map with attributes
   * @param key        the attribute key
   * @param optValue   the optional value of the attribute
   * @tparam A the data type of the attribute
   * @tparam B the data type of the default value
   * @return the modified map with attributes if a change was necessary
   */
  private def addOptionalAttribute[A <: AnyRef, B <: A](attributes: ParameterAttributes,
                                                        key: ParameterAttributeKey[A],
                                                        optValue: Option[B]): ParameterAttributes =
    optValue.fold(attributes)(value => attributes + (key, value))

  /**
   * A function to determine the signum of an index which can be either
   * positive or negative.
   *
   * @param i the input number
   * @return the signum of this number
   */
  private def sig(i: Int): Int =
    if (i < 0) -1 else 1
}
