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
import java.nio.charset.StandardCharsets
import java.nio.file.{FileVisitResult, Files, Path, SimpleFileVisitor}
import java.nio.file.attribute.BasicFileAttributes

import com.github.scli.ParameterModel.{ModelContext, ParameterAttributeKey, ParameterAttributes, ParameterKey}
import com.github.scli.ParameterParser.{AliasResolverFunc, CliClassifierFunc, CliElement, ExtractedKeyClassifierFunc, InputParameterElement, OptionElement, OptionPrefixes, ParameterFileException, ParametersMap, SwitchesElement}
import com.github.scli.ParametersTestHelper._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.SortedSet
import scala.util.{Failure, Success, Try}

object ParameterParserSpec {
  /** Name of an option that references parameter files. */
  private val FileOption = "param-file"

  /** Short name of an option that references parameter files. */
  private val ShortFileOption = "f"

  /** A list with all options referencing parameter files. */
  private val FileOptionKeys = List(ParameterKey(FileOption, shortAlias = false),
    ParameterKey(ShortFileOption, shortAlias = true))

  /** The prefix for temporary files created by this class. */
  private val TempFilePrefix = "FileTestHelper"

  /** The suffix for temporary files created by this class. */
  private val TempFileSuffix = ".tmp"

  /** A key for a test parameter. */
  private val TestKey = pk("theTestKey")

  /** Test value of the test parameter. */
  private val TestValue = "testValue"

  /** An alias key for the test parameter. */
  private val AliasKey = ParameterKey("k", shortAlias = true)

  /** A resolver function for aliases that never resolves an alias. */
  private val NoAliasResolverFunc: AliasResolverFunc = _ => None

  /**
   * Helper method for converting a string to a byte array.
   *
   * @param s the string
   * @return the byte array
   */
  private def toBytes(s: String): Array[Byte] = s.getBytes(StandardCharsets.UTF_8)

  /**
   * Returns a very simple classifier function that distinguishes between
   * options and input parameters.
   *
   * @return the basic classifier function
   */
  private def basicClassifierFunc: CliClassifierFunc =
    (args, idx) => {
      val arg = args(idx)
      val isOption = arg startsWith "-"
      val isLongOption = arg startsWith "--"
      if (isOption) {
        val keyPos = if (isLongOption) 2 else 1
        OptionElement(ParameterKey(arg.substring(keyPos), !isLongOption), Some(args(idx + 1)))
      }
      else InputParameterElement(arg)
    }

  /**
   * Returns a key classifier function that always throws an exception. This is
   * used to test that this function is not called.
   *
   * @return the classifier function that throws an exception
   */
  private def blowUpKeyClassifier: ExtractedKeyClassifierFunc =
    (_, _, _) => throw new UnsupportedOperationException("Unexpected invocation")

  /**
   * Creates a ''ModelContext'' with the given set of attributes for the test
   * option key.
   *
   * @param attributes the attributes for the test option key
   * @return the resulting model context
   */
  private def createModelContext(attributes: ParameterAttributes): ModelContext = {
    val optionAttributes = Map(TestKey -> attributes)
    val aliasMapping = ParameterModel.EmptyAliasMapping.addAlias(TestKey, AliasKey)
    new ModelContext(optionAttributes, SortedSet.empty, aliasMapping, None, Nil)
  }

  /**
   * Helper function to create a ''ParameterAttributes'' object with a single
   * value only.
   *
   * @param key   the attribute key
   * @param value the value of this attribute
   * @tparam A the data type of the attribute
   * @return the ''ParameterAttributes'' containing this attribute
   */
  private def singleAttr[A <: AnyRef](key: ParameterAttributeKey[A], value: A): ParameterAttributes =
    new ParameterAttributes + (key -> value)
}

/**
 * Test class for ''ParameterParser''.
 */
class ParameterParserSpec extends AnyFlatSpec with BeforeAndAfterEach with Matchers {

  import ParameterParserSpec._

  /** The test directory used by this test class. */
  private var optTestDirectory: Option[Path] = None

  override protected def afterEach(): Unit = {
    tearDownTestDirectory()
    super.afterEach()
  }

  /**
   * Removes the temporary directory and its content if it exists.
   */
  private def tearDownTestDirectory(): Unit = {
    optTestDirectory foreach deleteTree
    optTestDirectory = None
  }

  /**
   * Deletes a directory and all its content.
   *
   * @param path the directory to be deleted
   */
  private def deleteDirectory(path: Path): Unit = {
    Files.walkFileTree(path, new SimpleFileVisitor[Path] {
      override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
        Files delete file
        FileVisitResult.CONTINUE
      }

      override def postVisitDirectory(dir: Path, exc: IOException): FileVisitResult = {
        if (exc == null) {
          Files delete dir
          FileVisitResult.CONTINUE
        } else throw exc
      }
    })
  }

  /**
   * Deletes a whole directory tree. If the specified path is a directory, it
   * is removed with all its content. If it is only a single file, it is
   * directly removed.
   *
   * @param path the path in question
   */
  private def deleteTree(path: Path): Unit = {
    if (Files exists path) {
      if (Files.isDirectory(path))
        deleteDirectory(path)
      else Files delete path
    }
  }

  /**
   * Checks whether the temporary directory has been created. If not, it is
   * created now. The corresponding path is returned.
   *
   * @return the path to the managed temporary directory
   */
  private def ensureTempDirectory(): Path = {
    optTestDirectory = optTestDirectory orElse Some(createTempDirectory())
    optTestDirectory.get
  }

  /**
   * Creates a temporary directory which is the root folder of all temporary
   * files created by this trait.
   *
   * @return the temporary directory
   */
  private def createTempDirectory(): Path =
    Files createTempDirectory TempFilePrefix

  /**
   * Creates a new temporary file reference with no content.
   *
   * @return the path to the new file
   */
  def createFileReference(): Path =
    Files.createTempFile(ensureTempDirectory(), TempFilePrefix, TempFileSuffix)

  /**
   * Writes the given content in a file specified by the given path.
   *
   * @param path    the path of the file
   * @param content the content to be written
   * @return the path to the file that was written
   */
  def writeFileContent(path: Path, content: String): Path = {
    Files.createDirectories(path.getParent)
    Files.write(path, toBytes(content))
    path
  }

  /**
   * Creates a new temporary file physically on disk which has the specified content.
   *
   * @param content the content of the file
   * @return the path to the new file
   */
  def createDataFile(content: String): Path = {
    val path = createFileReference()
    writeFileContent(path, content)
  }

  /**
   * Generates a classifier function that returns defined results.
   *
   * @param args    the command line arguments
   * @param results the results to return for arguments
   * @return the classifier function
   */
  private def classifierFunc(args: Seq[String], results: Seq[CliElement]): CliClassifierFunc =
    (params, idx) => {
      params should be(args)
      results(idx)
    }

  /**
   * Invokes the parameter parser on the given sequence with arguments ignoring
   * aliases and returns the result.
   *
   * @param args the sequence with arguments
   * @param cf   the classifier function
   * @return the resulting parameters map
   */
  private def parseParametersSuccess(args: Seq[String])(cf: CliClassifierFunc): ParametersMap =
    ParameterParser.parseParameters(args)(cf)(NoAliasResolverFunc)

  /**
   * Invokes the parameter parser to resolve file options on the given sequence
   * of arguments. Uses a default classifier and file options function.
   *
   * @param args the sequence of arguments
   * @return the resulting processed sequence of arguments
   */
  private def processFileOptions(args: Seq[String]): Try[Seq[String]] = {
    val fileOptionFunc = ParameterParser.fileOptionFuncForOptions(FileOptionKeys)
    ParameterParser.processParameterFiles(args)(basicClassifierFunc)(fileOptionFunc)
  }

  /**
   * Invokes the parameter parser to resolve file options and returns the
   * success result or fails miserably.
   *
   * @param args the sequence of arguments
   * @return the resulting processed sequence of arguments
   */
  private def processFileOptionsSuccess(args: Seq[String]): Seq[String] =
    processFileOptions(args) match {
      case Success(result) => result
      case r => fail("Unexpected result: " + r)
    }

  /**
   * Creates a temporary file that contains the given parameter strings.
   *
   * @param args the parameters to store in the file
   * @return the path to the newly created file
   */
  private def createParameterFile(args: String*): Path =
    createDataFile(parameterFileContent(args: _*))

  /**
   * Generates the content of a parameters file from the given parameter
   * strings.
   *
   * @param args the parameters to store in the file
   * @return the content of the parameter file as string
   */
  private def parameterFileContent(args: String*): String =
    args.mkString("\r\n")

  /**
   * Adds a parameter to read the given file to a parameter list.
   *
   * @param path       the path to the file to be read
   * @param argList    the original parameter list
   * @param shortAlias flag whether the short alias key should be used
   * @return the parameter list with the file parameter added
   */
  private def appendFileParameter(path: Path, argList: List[String], shortAlias: Boolean = false): List[String] = {
    val optKey = if (shortAlias) "-" + ShortFileOption
    else "--" + FileOption
    optKey :: path.toString :: argList
  }

  /**
   * Returns a key classifier function that expects the given key to be passed
   * in and returns the given result.
   *
   * @param expKey  the expected key
   * @param expArgs the expected sequence of arguments
   * @param expIdx  the expected index in the arguments sequence
   * @param result  the result to return
   * @return the key classifier function with these properties
   */
  private def definedKeyClassifier(expKey: ParameterKey, expArgs: Seq[String], expIdx: Int, result: Option[CliElement]):
  ExtractedKeyClassifierFunc =
    (key, args, idx) => {
      key should be(expKey)
      args should be(expArgs)
      idx should be(expIdx)
      result
    }

  "OptionPrefixes" should "extract keys from valid options" in {
    val Options = List("--foo", "-foo", "/foo", "<<<<foo")
    val prefixes = OptionPrefixes(pk("-"), pk("/"), pk("<<<<"), pk("--"))

    Options.forall(prefixes.tryExtract(_).contains(pk("foo"))) shouldBe true
  }

  it should "return empty keys for non-option parameters" in {
    val Params = List("-nope", "neither", "-+alsoNot", "/fullWrong")
    val prefixes = OptionPrefixes(pk("--"))

    Params.forall(prefixes.tryExtract(_).isEmpty) shouldBe true
  }

  it should "support special prefixes for short alias names" in {
    val Key = ParameterKey("d", shortAlias = true)
    val prefixes = OptionPrefixes(pk("--"), ParameterKey("-", shortAlias = true))

    val optExtract = prefixes.tryExtract("-" + Key.key)
    optExtract should be(Some(Key))
  }

  "CliElement" should "report the value for options" in {
    val elem = OptionElement(TestKey, Some(TestValue))

    elem.value should be(TestValue)
  }

  it should "report the value for options if no value is defined" in {
    val elem = OptionElement(TestKey, None)

    elem.value should be("")
  }

  it should "report the key for switches" in {
    val elem = SwitchesElement(List((TestKey, TestValue), (pk("foo"), "aValue")))

    elem.key should be(TestKey)
  }

  it should "report the value for switches" in {
    val elem = SwitchesElement(List((TestKey, TestValue), (pk("foo"), "aValue")))

    elem.value should be(TestValue)
  }

  it should "report the key for input parameters" in {
    val elem = InputParameterElement(TestValue)

    elem.key should be(ParameterParser.InputParameter)
  }

  it should "report the value for input parameters" in {
    val elem = InputParameterElement(TestValue)

    elem.value should be(TestValue)
  }

  "ParameterParser" should "parse an empty sequence of arguments" in {
    val cf: CliClassifierFunc = (_, _) => throw new UnsupportedOperationException("Unexpected invocation")
    val params = parseParametersSuccess(Nil)(cf)

    params should have size 0
  }

  it should "correctly parse non-option parameters" in {
    val args = List("uri1", "uri2")
    val elements = args map (v => InputParameterElement(v))
    val cf = classifierFunc(args, elements)
    val expArgMap = Map(ParameterParser.InputParameter -> args)

    val params = parseParametersSuccess(args)(cf)
    params should be(expArgMap)
  }

  it should "correctly parse arguments with options" in {
    val args = List("--opt1", "opt1Val1", "--opt2", "opt2Val1", "--opt1", "opt1Val2")
    val elements = List(OptionElement(pk("opt1"), Some("opt1Val1")),
      null, OptionElement(pk("opt2"), Some("opt2Val1")),
      null, OptionElement(pk("opt1"), Some("opt1Val2")))
    val cf = classifierFunc(args, elements)
    val expArgMap = Map(pk("opt1") -> List("opt1Val1", "opt1Val2"),
      pk("opt2") -> List("opt2Val1"))

    val params = parseParametersSuccess(args)(cf)
    params should be(expArgMap)
  }

  it should "correctly evaluate the shortAlias flag for options" in {
    val Key = ParameterKey("f", shortAlias = true)
    val args = List("--" + Key.key, TestValue)
    val elements = List(OptionElement(Key, Some(TestValue)))
    val cf = classifierFunc(args, elements)
    val expArgMap = Map(Key -> List(TestValue))

    val params = parseParametersSuccess(args)(cf)
    params should be(expArgMap)
  }

  it should "ignore an option that is the last argument" in {
    val undefOption = "undefinedOption"
    val args = List("--opt1", "optValue", "--" + undefOption)
    val elements = List(OptionElement(pk("opt1"), Some("optValue")), null,
      OptionElement(pk(undefOption), None))
    val cf = classifierFunc(args, elements)
    val expArgsMap = Map(pk("opt1") -> List("optValue"))

    val params = parseParametersSuccess(args)(cf)
    params should be(expArgsMap)
  }

  it should "ignore an option that is the last argument, but keep its other values" in {
    val Key = pk("strangeOption")
    val TestOption = "--" + Key.key
    val args = List(TestOption, "v1", TestOption, "v2", TestOption)
    val elements = List(OptionElement(Key, Some("v1")), null, OptionElement(Key, Some("v2")),
      null, OptionElement(Key, None))
    val cf = classifierFunc(args, elements)
    val expArgsMap = Map(Key -> List("v1", "v2"))

    val params = parseParametersSuccess(args)(cf)
    params should be(expArgsMap)
  }

  it should "correctly parse arguments with switches" in {
    val args = List("--v", "other", "--xzvf", "--flag")
    val elements = List(SwitchesElement(List((pk("v"), "true"))), InputParameterElement("other"),
      SwitchesElement(List((pk("x"), "true"), (pk("z"), "false"), (pk("v"), "true"),
        (pk("f"), "true"))),
      SwitchesElement(List((pk("flag"), "false"))))
    val cf = classifierFunc(args, elements)
    val expArgsMap = Map(ParameterParser.InputParameter -> List("other"),
      pk("v") -> List("true", "true"), pk("x") -> List("true"), pk("z") -> List("false"),
      pk("f") -> List("true"), pk("flag") -> List("false"))

    val params = parseParametersSuccess(args)(cf)
    params should be(expArgsMap)
  }

  it should "correctly evaluate the shortAlias flag for switches" in {
    val Key = ParameterKey("f", shortAlias = true)
    val args = List("--v")
    val elements = List(SwitchesElement(List((Key, "true"))))
    val cf = classifierFunc(args, elements)
    val expArgsMap = Map(Key -> List("true"))

    val params = parseParametersSuccess(args)(cf)
    params should be(expArgsMap)
  }

  it should "resolve an alias for an option" in {
    val AliasKey = ParameterKey("o", shortAlias = true)
    val AliasValue = "anotherValue"
    val args = List("--" + TestKey.key, TestValue, "-" + AliasKey.key, AliasValue)
    val elements = List(OptionElement(TestKey, Some(TestValue)), null, OptionElement(AliasKey, Some(AliasValue)))
    val cf = classifierFunc(args, elements)
    val expArgsMap = Map(TestKey -> List(TestValue, AliasValue))

    val params = ParameterParser.parseParameters(args)(cf) { key =>
      if (key == AliasKey) Some(TestKey)
      else None
    }
    params should be(expArgsMap)
  }

  it should "resolve aliases for switches" in {
    val AliasKey = ParameterKey("s", shortAlias = true)
    val Key2 = pk("flag")
    val AliasKey2 = ParameterKey("f", shortAlias = true)
    val args = List("-" + AliasKey.key + AliasKey2.key)
    val elements = List(SwitchesElement(List((AliasKey, "true"), (AliasKey2, "false"))))
    val cf = classifierFunc(args, elements)
    val AliasMapping = Map(AliasKey -> TestKey, AliasKey2 -> Key2)
    val expArgsMap = Map(TestKey -> List("true"), Key2 -> List("false"))

    val params = ParameterParser.parseParameters(args)(cf)(AliasMapping.get)
    params should be(expArgsMap)
  }

  it should "process file options if there are none" in {
    val args = List("larry", "curly", "moe", "--funny", "true")

    val processedArgs = processFileOptionsSuccess(args)
    processedArgs should be(args)
  }

  it should "add the content of parameter files to command line options" in {
    val OptionName1 = "foo"
    val OptionName2 = "test"
    val Opt1Val1 = "bar"
    val Opt1Val2 = "baz"
    val Opt2Val = "true"
    val uri1 = "testUri1"
    val uri2 = "testUri2"
    val args = appendFileParameter(createParameterFile("--" + OptionName1, Opt1Val1, uri1),
      appendFileParameter(createParameterFile("--" + OptionName2, Opt2Val),
        "--" + OptionName1 :: Opt1Val2 :: uri2 :: Nil, shortAlias = true))
    val expResult = List("--" + OptionName1, Opt1Val1, uri1, "--" + OptionName2, Opt2Val, "--" + OptionName1,
      Opt1Val2, uri2)

    val processedArgs = processFileOptionsSuccess(args)
    processedArgs should contain theSameElementsInOrderAs expResult
  }

  it should "parse parameter files defined in another parameter file" in {
    val OptionName1 = "top-level"
    val Option1Value = "onCommandLine"
    val OptionName2 = "level1"
    val Option2Value = "inFirstFile"
    val OptionName3 = "deep"
    val Option3Value = "inNestedFile"
    val nestedFile = createParameterFile("--" + OptionName3, Option3Value)
    val args = appendFileParameter(createParameterFile("--" + OptionName2, Option2Value,
      "--" + FileOption, nestedFile.toString), "--" + OptionName1 :: Option1Value :: Nil)
    val expArgs = List("--" + OptionName2, Option2Value, "--" + OptionName3, Option3Value,
      "--" + OptionName1, Option1Value)

    val processedArgs = processFileOptionsSuccess(args)
    processedArgs should contain theSameElementsInOrderAs expArgs
  }

  it should "deal with cyclic references in parameter files" in {
    val file1 = createFileReference()
    val file3 = createParameterFile("--" + FileOption, file1.toString, "--op3", "v3")
    val file2 = createParameterFile("-" + ShortFileOption, file3.toString, "--op2", "v2")
    writeFileContent(file1, parameterFileContent("--" + FileOption, file2.toString,
      "--op1", "v1", "--" + FileOption, file2.toString))
    val args = appendFileParameter(file1, Nil)
    val expArgs = List("--op1", "v1", "--op3", "v3", "--op2", "v2")

    val processedArgs = processFileOptionsSuccess(args)
    processedArgs should contain theSameElementsInOrderAs expArgs
  }

  it should "ignore empty lines in parameter files" in {
    val args = appendFileParameter(createParameterFile("--foo", "bar", "", "--foo", "baz"),
      "--test" :: "true" :: Nil)
    val expArgs = List("--foo", "bar", "--foo", "baz", "--test", "true")

    val processedArgs = processFileOptionsSuccess(args)
    processedArgs should contain theSameElementsInOrderAs expArgs
  }

  it should "handle an exception when reading a parameter file" in {
    val FileName = "non_existing_file.txt"
    val args = List("--op1", "don't care", "--" + FileOption, FileName)

    processFileOptions(args) match {
      case Failure(e: ParameterFileException) =>
        e.getMessage should include(FileName)
        e.getCause shouldBe a[IOException]
        e.fileOption should be(pk(FileOption))
      case r => fail("Unexpected result: " + r)
    }
  }

  it should "create a classifier function that handles input parameters" in {
    val args = List("foo")
    val cf = ParameterParser.classifierOf(List(blowUpKeyClassifier))(ParameterParser.DefaultOptionPrefixes.tryExtract)

    cf(args, 0) should be(InputParameterElement(args.head))
  }

  it should "create a classifier function with an empty list of key classifiers" in {
    val args = List("foo", "bar")
    val cf = ParameterParser.classifierOf(List.empty)(ParameterParser.DefaultOptionPrefixes.tryExtract)

    cf(args, 1) should be(InputParameterElement(args(1)))
  }

  it should "create a classifier function that handles key classifiers returning None" in {
    val Key = pk("arg")
    val args = List("--" + Key.key)
    val cf = ParameterParser.classifierOf(List(definedKeyClassifier(Key, args, 0, None),
      definedKeyClassifier(Key, args, 0, None)))(ParameterParser.DefaultOptionPrefixes.tryExtract)

    cf(args, 0) should be(InputParameterElement(args.head))
  }

  it should "create a classifier function that correctly delegates to key classifiers" in {
    val Key = ParameterKey("theOptionKey", shortAlias = true)
    val Result = OptionElement(Key, Some("someValue"))
    val args = List("test", "-" + Key.key)
    val cf = ParameterParser.classifierOf(List(definedKeyClassifier(Key, args, 1, None),
      definedKeyClassifier(Key, args, 1, Some(Result)),
      blowUpKeyClassifier))(ParameterParser.DefaultOptionPrefixes.tryExtract)

    cf(args, 1) should be(Result)
  }

  it should "provide a key classifier for options that handles an option key" in {
    val args = List("someKey", TestValue)
    val context = createModelContext(singleAttr(ParameterModel.AttrParameterType, ParameterModel.ParameterTypeOption))

    val classifier = ParameterParser.optionKeyClassifierFunc(context)(NoAliasResolverFunc)
    classifier(TestKey, args, 0) should be(Some(OptionElement(TestKey, Some(TestValue))))
  }

  it should "provide a key classifier for options that checks the option type" in {
    val args = List("someKey", TestValue)
    val context = createModelContext(singleAttr(ParameterModel.AttrParameterType, ParameterModel.ParameterTypeSwitch))

    val classifier = ParameterParser.optionKeyClassifierFunc(context)(NoAliasResolverFunc)
    classifier(TestKey, args, 0) should be(None)
  }

  it should "provide a key classifier for options that handles a missing attribute" in {
    val args = List("someKey", "someOtherKey", TestValue)
    val context = createModelContext(singleAttr(ParameterModel.AttrGroup, Set("someGroup")))

    val classifier = ParameterParser.optionKeyClassifierFunc(context)(NoAliasResolverFunc)
    classifier(TestKey, args, 1) should be(None)
  }

  it should "provide a key classifier for options that handles an unknown parameter key" in {
    val UnknownKey = pk("unknownKey")
    val args = List("someKey", "someOtherKey", TestValue)
    val context = createModelContext(singleAttr(ParameterModel.AttrParameterType, ParameterModel.ParameterTypeOption))

    val classifier = ParameterParser.optionKeyClassifierFunc(context)(NoAliasResolverFunc)
    classifier(UnknownKey, args, 1) should be(None)
  }

  it should "provide a key classifier for options that handles a missing option value" in {
    val args = List("key1", "key2", TestKey.key)
    val context = createModelContext(singleAttr(ParameterModel.AttrParameterType, ParameterModel.ParameterTypeOption))

    val classifier = ParameterParser.optionKeyClassifierFunc(context)(NoAliasResolverFunc)
    classifier(TestKey, args, 2) should be(Some(OptionElement(TestKey, None)))
  }

  it should "provide a key classifier for switches that handles a switch key" in {
    val attributes = new ParameterAttributes +
      (ParameterModel.AttrParameterType -> ParameterModel.ParameterTypeSwitch) +
      (ParameterModel.AttrSwitchValue -> "true")
    val context = createModelContext(attributes)

    val classifier = ParameterParser.switchKeyClassifierFunc(context)(NoAliasResolverFunc)
    classifier(TestKey, Nil, 0) should be(Some(SwitchesElement(List((TestKey, "true")))))
  }

  it should "provide a key classifier for switches that evaluates the value of the switch" in {
    val attributes = new ParameterAttributes +
      (ParameterModel.AttrParameterType -> ParameterModel.ParameterTypeSwitch) +
      (ParameterModel.AttrSwitchValue -> "false")
    val context = createModelContext(attributes)

    val classifier = ParameterParser.switchKeyClassifierFunc(context)(NoAliasResolverFunc)
    classifier(TestKey, Nil, 0) should be(Some(SwitchesElement(List((TestKey, "false")))))
  }

  it should "provide a key classifier for switches that evaluates the parameter type" in {
    val context = createModelContext(singleAttr(ParameterModel.AttrParameterType, ParameterModel.ParameterTypeOption))

    val classifier = ParameterParser.switchKeyClassifierFunc(context)(NoAliasResolverFunc)
    classifier(TestKey, Nil, 0) should be(None)
  }

  it should "provide a key classifier for switches that handles an unknown parameter key" in {
    val context = createModelContext(new ParameterAttributes)

    val classifier = ParameterParser.switchKeyClassifierFunc(context)(NoAliasResolverFunc)
    classifier(TestKey, Nil, 0) should be(None)
  }

  it should "provide a key classifier for switches that handles a missing switch value attribute" in {
    val context = createModelContext(singleAttr(ParameterModel.AttrParameterType, ParameterModel.ParameterTypeSwitch))

    val classifier = ParameterParser.switchKeyClassifierFunc(context)(NoAliasResolverFunc)
    classifier(TestKey, Nil, 0) should be(Some(SwitchesElement(List((TestKey, "true")))))
  }

  it should "handle aliases when accessing metadata during classification" in {
    val attributes = new ParameterAttributes +
      (ParameterModel.AttrParameterType -> ParameterModel.ParameterTypeSwitch) +
      (ParameterModel.AttrSwitchValue -> "false")
    val context = createModelContext(attributes)

    val classifier = ParameterParser.switchKeyClassifierFunc(context)(context.aliasMapping.keyForAlias.get)
    classifier(AliasKey, Nil, 0) should be(Some(SwitchesElement(List((AliasKey, "false")))))
  }

  it should "provide a multi key classifier for switches that handles long switch keys" in {
    val attributes = new ParameterAttributes +
      (ParameterModel.AttrParameterType -> ParameterModel.ParameterTypeSwitch) +
      (ParameterModel.AttrSwitchValue -> "true")
    val context = createModelContext(attributes)

    val classifier = ParameterParser.combinedSwitchKeyClassifierFunc(context)(NoAliasResolverFunc)
    classifier(TestKey, Nil, 0) should be(Some(SwitchesElement(List((TestKey, "true")))))
  }

  it should "provide a multi key classifier for switches that handles long keys for non-switches" in {
    val context = createModelContext(singleAttr(ParameterModel.AttrParameterType, ParameterModel.ParameterTypeOption))

    val classifier = ParameterParser.combinedSwitchKeyClassifierFunc(context)(NoAliasResolverFunc)
    classifier(TestKey, Nil, 0) should be(None)
  }

  it should "provide a multi key classifier for switches that extracts multiple keys" in {
    val AliasKey2 = ParameterKey("x", shortAlias = true)
    val multiKey = ParameterKey(AliasKey.key + AliasKey2.key, shortAlias = true)
    val expElement = SwitchesElement(List((AliasKey, "false"), (AliasKey2, "true")))
    val context = createModelContext(singleAttr(ParameterModel.AttrSwitchValue, "false"))

    val classifier = ParameterParser.combinedSwitchKeyClassifierFunc(context)(context.aliasMapping.keyForAlias.get)
    classifier(multiKey, Nil, 0) should be(Some(expElement))
  }
}
