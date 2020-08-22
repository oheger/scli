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
  final val AttrHelpText = "helpText"

  /** The attribute with a description for the fallback value. */
  final val AttrFallbackValue = "fallbackValue"

  /** The attribute defining the multiplicity of an option. */
  final val AttrMultiplicity = "multiplicity"

  /**
   * The attribute assigning a group to an option. Groups are used to handle
   * options that are valid only in certain constellations, e.g. when
   * conditional extractors are involved, or if a CLI supports multiple
   * commands, each of which has its own set of options. In the help text it
   * can then be indicated that the options belonging to a group are allowed
   * only if specific conditions are fulfilled.
   */
  final val AttrGroup = "group"

  /**
   * The attribute defining the type of a parameter. This attribute contains
   * the information whether a parameter is an option, a switch, or an input
   * parameter.
   */
  final val AttrParameterType = "parameterType"

  /**
   * The attribute to store an error message for an option. The attribute can
   * be set if parameter extraction has failed for an option. It is then
   * possible to generate output containing all error messages using all the
   * column generators supported.
   */
  final val AttrErrorMessage = "errorMessage"

  /**
   * The attribute defining the value to be assigned to a switch if it is
   * present. This is normally a boolean constant.
   */
  final val AttrSwitchValue = "switchValue"

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

  /** The separator string between group names. */
  final val GroupSeparator = ","

  /** Constant for an initial, empty mapping for parameter aliases. */
  final val EmptyAliasMapping = AliasMapping(Map.empty, Map.empty)

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
      val updatedAliasList = aliasList :+ alias
      AliasMapping(aliasesForKey + (key -> updatedAliasList),
        keyForAlias + (alias -> key))
    }
  }

  /**
   * A data class storing information about a single command line parameter.
   *
   * An instance of this class contains meta data about a parameter  that can
   * be used for various purposes, e.g. to generate the help text of a CLI
   * application. Some of the attributes are also used to implement specific
   * features during command line processing. The meta data is extensible and
   * is therefore stored as a map of attributes (whose values are strings as
   * they are expected in many cases to be printed on the console).
   *
   * @param attributes a map with attributes for the associated parameter
   */
  case class ParameterAttributes(attributes: Map[String, String])

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
                     groups: List[String]) {

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
     * @return the updated ''ModelContext''
     */
    def addAttribute(attrKey: String, value: String): ModelContext =
      updateWithCurrentKey { key =>
        val attrs = options(key)
        val newAttrs = ParameterAttributes(attrs.attributes + (attrKey -> value))
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
    def hasAttribute(key: ParameterKey, attrKey: String): Boolean =
      options.get(key) exists (_.attributes contains attrKey)

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
     * Returns an ''Iterable'' with ''ParameterMetaData'' objects for the options
     * stored in this context.
     *
     * @return an iterable with meta data about all options in this context
     */
    def parameterMetaData: Iterable[ParameterMetaData] =
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
      val existingAttrs = options.get(key).map(_.attributes) getOrElse Map.empty
      val existingGroups = existingAttrs.getOrElse(AttrGroup, "")
      val attrs = addOptionalAttribute(
        addOptionalAttribute(Map.empty, AttrHelpText, text),
        AttrGroup, groupAttribute.map(existingGroups + _)) + (AttrParameterType -> optionType)
      val help = ParameterAttributes(existingAttrs ++ attrs)
      copy(options = options + (key -> help), inputs = inputRefs, optCurrentKey = Some(key))
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
     * Returns a string with the concatenated names of all groups that are
     * currently active.
     *
     * @return a string with the names of the active groups
     */
    private def activeGroupNames: String = groups.mkString(GroupSeparator) + GroupSeparator

    /**
     * Returns an ''Option'' with the current value of the group attribute. If
     * there are active groups, the ''Option'' contains their names;
     * otherwise, it is empty.
     *
     * @return an ''Option'' with the names of the currently active groups
     */
    private def groupAttribute: Option[String] =
      activeGroupNames match {
        case GroupSeparator => None
        case s => Some(s)
      }

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
   * Adds an attribute and its value to the given map of attributes only if
   * the value is defined.
   *
   * @param attributes the map with attributes
   * @param key        the attribute key
   * @param optValue   the optional value of the attribute
   * @return the modified map with attributes if a change was necessary
   */
  private def addOptionalAttribute(attributes: Map[String, String], key: String, optValue: Option[String]):
  Map[String, String] =
    optValue map (value => attributes + (key -> value)) getOrElse attributes

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
