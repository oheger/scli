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

import com.github.scli.ParameterModel.{ModelContext, ParameterAttributes}
import com.github.scli.ParameterParser.{CliClassifierFunc, CliElement, ExtractedKeyClassifierFunc, InputParameterElement, OptionElement, OptionPrefixes, ParameterParseException, SwitchesElement}
import org.scalatest.BeforeAndAfterEach
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.SortedSet
import scala.util.{Failure, Success, Try}

object ParameterParserSpec {
  /** Name of an option that references parameter files. */
  private val FileOption = "param-file"

  /** The prefix for temporary files created by this class. */
  private val TempFilePrefix = "FileTestHelper"

  /** The suffix for temporary files created by this class. */
  private val TempFileSuffix = ".tmp"

  /** A key for a test parameter. */
  private val TestKey = "theTestKey"

  /** Test value of the test parameter. */
  private val TestValue = "testValue"

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
      if (arg.startsWith("--")) OptionElement(arg.substring(2), Some(args(idx + 1)))
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
  private def createModelContext(attributes: Map[String, String]): ModelContext = {
    val optionAttributes = Map(TestKey -> ParameterAttributes(attributes))
    new ModelContext(optionAttributes, SortedSet.empty, None, Nil)
  }
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
   * Extracts the map with parameters from the given tried result; fails for
   * other results.
   *
   * @param result the result
   * @return the map with parameters
   */
  private def extractParametersMap(result: Try[ParameterParser.ParametersMap]): ParameterParser.ParametersMap =
    result match {
      case Success(value) => value
      case r => fail("Unexpected result: " + r)
    }

  /**
   * Invokes the parameter parser on the given sequence with arguments and
   * expects a successful result. The resulting map is returned.
   *
   * @param args the sequence with arguments
   * @param cf   the classifier function
   * @return the resulting parameters map
   */
  private def parseParametersSuccess(args: Seq[String])(cf: CliClassifierFunc): ParameterParser.ParametersMap =
    extractParametersMap(ParameterParser.parseParameters(args, optFileOption = Some(FileOption))(cf))

  /**
   * Invokes the parameter parser on the given sequence with arguments and
   * expects a failure result. The causing exception is returned.
   *
   * @param args the sequence with arguments
   * @param cf   the classifier function
   * @return the exception causing the failure
   */
  private def parseParametersFailure(args: Seq[String])(cf: CliClassifierFunc): Throwable =
    ParameterParser.parseParameters(args, optFileOption = Some(FileOption))(cf) match {
      case Failure(exception) => exception
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
   * @param path    the path to the file to be read
   * @param argList the original parameter list
   * @return the parameter list with the file parameter added
   */
  private def appendFileParameter(path: Path, argList: List[String]): List[String] =
    "--" + FileOption :: path.toString :: argList

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
  private def definedKeyClassifier(expKey: String, expArgs: Seq[String], expIdx: Int, result: Option[CliElement]):
  ExtractedKeyClassifierFunc =
    (key, args, idx) => {
      key should be(expKey)
      args should be(expArgs)
      idx should be(expIdx)
      result
    }

  "OptionPrefixes" should "extract keys from valid options" in {
    val Options = List("--foo", "-foo", "/foo", "<<<<foo")
    val prefixes = OptionPrefixes("-", "/", "<<<<", "--")

    Options.forall(prefixes.tryExtract(_).contains("foo")) shouldBe true
  }

  it should "return empty keys for non-option parameters" in {
    val Params = List("-nope", "neither", "-+alsoNot", "/fullWrong")
    val prefixes = OptionPrefixes("--")

    Params.forall(prefixes.tryExtract(_).isEmpty) shouldBe true
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
    val expArgMap = Map(ParameterParser.InputOption -> args)

    val params = parseParametersSuccess(args)(cf)
    params should be(expArgMap)
  }

  it should "correctly parse arguments with options" in {
    val args = List("--opt1", "opt1Val1", "--opt2", "opt2Val1", "--opt1", "opt1Val2")
    val elements = List(OptionElement("opt1", Some("opt1Val1")),
      null, OptionElement("opt2", Some("opt2Val1")),
      null, OptionElement("opt1", Some("opt1Val2")))
    val cf = classifierFunc(args, elements)
    val expArgMap = Map("opt1" -> List("opt1Val1", "opt1Val2"),
      "opt2" -> List("opt2Val1"))

    val params = parseParametersSuccess(args)(cf)
    params should be(expArgMap)
  }

  it should "ignore an option that is the last argument" in {
    val undefOption = "undefinedOption"
    val args = List("--opt1", "optValue", "--" + undefOption)
    val elements = List(OptionElement("opt1", Some("optValue")), null, OptionElement(undefOption, None))
    val cf = classifierFunc(args, elements)
    val expArgsMap = Map("opt1" -> List("optValue"))

    val params = parseParametersSuccess(args)(cf)
    params should be(expArgsMap)
  }

  it should "ignore an option that is the last argument, but keep its other values" in {
    val Key = "strangeOption"
    val TestOption = "--" + Key
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
    val elements = List(SwitchesElement(List(("v", "true"))), InputParameterElement("other"),
      SwitchesElement(List(("x", "true"), ("z", "false"), ("v", "true"), ("f", "true"))),
      SwitchesElement(List(("flag", "false"))))
    val cf = classifierFunc(args, elements)
    val expArgsMap = Map(ParameterParser.InputOption -> List("other"),
      "v" -> List("true", "true"), "x" -> List("true"), "z" -> List("false"),
      "f" -> List("true"), "flag" -> List("false"))

    val params = parseParametersSuccess(args)(cf)
    params should be(expArgsMap)
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
        "--" + OptionName1 :: Opt1Val2 :: uri2 :: Nil))

    val argsMap = parseParametersSuccess(args)(basicClassifierFunc)
    argsMap(OptionName1) should contain only(Opt1Val1, Opt1Val2)
    argsMap(OptionName2) should contain only Opt2Val
    argsMap.keys should not contain FileOption
  }

  it should "parse parameter files defined in another parameter file" in {
    val OptionName1 = "top-level"
    val Option1Value = "onCommandLine"
    val OptionName2 = "level1"
    val Option2Value = "inFirstFile"
    val OptionName3 = "deep"
    val Option3Value = "inNestedFile"
    val nestedFile = createParameterFile("--" + OptionName3, Option3Value)
    val args = appendFileParameter(
      createParameterFile("--" + FileOption, nestedFile.toString,
        "--" + OptionName2, Option2Value), "--" + OptionName1 :: Option1Value :: Nil)
    val expArgs = Map(OptionName1 -> List(Option1Value),
      OptionName2 -> List(Option2Value),
      OptionName3 -> List(Option3Value))

    val argsMap = parseParametersSuccess(args)(basicClassifierFunc)
    argsMap should be(expArgs)
  }

  it should "deal with cyclic references in parameter files" in {
    val file1 = createFileReference()
    val file3 = createParameterFile("--" + FileOption, file1.toString, "--op3", "v3")
    val file2 = createParameterFile("--" + FileOption, file3.toString, "--op2", "v2")
    writeFileContent(file1, parameterFileContent("--" + FileOption, file2.toString,
      "--op1", "v1", "--" + FileOption, file2.toString))
    val args = appendFileParameter(file1, Nil)
    val expArgs = Map("op1" -> List("v1"), "op2" -> List("v2"), "op3" -> List("v3"))

    val argsMap = parseParametersSuccess(args)(basicClassifierFunc)
    argsMap should be(expArgs)
  }

  it should "ignore empty lines in parameter files" in {
    val args = appendFileParameter(createParameterFile("--foo", "bar", "", "--foo", "baz"),
      "--test" :: "true" :: Nil)

    val argsMap = parseParametersSuccess(args)(basicClassifierFunc)
    argsMap.keys should contain only("foo", "test")
  }

  it should "handle an exception when reading a parameter file" in {
    val FileName = "non_existing_file.txt"
    val args = List("--op1", "don't care", "--" + FileOption, FileName)
    val expArgs = Map("op1" -> List("don't care"))

    val exception = parseParametersFailure(args)(basicClassifierFunc)
    exception match {
      case e: ParameterParseException =>
        e.getMessage should include(FileName)
        e.getCause shouldBe a[IOException]
        e.fileOption should be(FileOption)
        e.currentParameters should be(expArgs)
      case e => fail("Unexpected exception: " + e)
    }
  }

  it should "create a classifier function that handles input parameters" in {
    val args = List("foo")
    val cf = ParameterParser.classifierOf(blowUpKeyClassifier)(ParameterParser.DefaultOptionPrefixes.tryExtract)

    cf(args, 0) should be(InputParameterElement(args.head))
  }

  it should "create a classifier function with an empty list of key classifiers" in {
    val args = List("foo", "bar")
    val cf = ParameterParser.classifierOf()(ParameterParser.DefaultOptionPrefixes.tryExtract)

    cf(args, 1) should be(InputParameterElement(args(1)))
  }

  it should "create a classifier function that handles key classifiers returning None" in {
    val Key = "arg"
    val args = List("--" + Key)
    val cf = ParameterParser.classifierOf(definedKeyClassifier(Key, args, 0, None),
      definedKeyClassifier(Key, args, 0, None))(ParameterParser.DefaultOptionPrefixes.tryExtract)

    cf(args, 0) should be(InputParameterElement(args.head))
  }

  it should "create a classifier function that correctly delegates to key classifiers" in {
    val Key = "theOptionKey"
    val Result = OptionElement(Key, Some("someValue"))
    val args = List("test", "--" + Key)
    val cf = ParameterParser.classifierOf(definedKeyClassifier(Key, args, 1, None),
      definedKeyClassifier(Key, args, 1, Some(Result)),
      blowUpKeyClassifier)(ParameterParser.DefaultOptionPrefixes.tryExtract)

    cf(args, 1) should be(Result)
  }

  it should "provide a key classifier for options that handles an option key" in {
    val args = List("someKey", TestValue)
    val context = createModelContext(Map(ParameterModel.AttrParameterType -> ParameterModel.ParameterTypeOption))

    val classifier = ParameterParser.optionKeyClassifierFunc(context)
    classifier(TestKey, args, 0) should be(Some(OptionElement(TestKey, Some(TestValue))))
  }

  it should "provide a key classifier for options that checks the option type" in {
    val args = List("someKey", TestValue)
    val context = createModelContext(Map(ParameterModel.AttrParameterType -> ParameterModel.ParameterTypeSwitch))

    val classifier = ParameterParser.optionKeyClassifierFunc(context)
    classifier(TestKey, args, 0) should be(None)
  }

  it should "provide a key classifier for options that handles a missing attribute" in {
    val args = List("someKey", "someOtherKey", TestValue)
    val context = createModelContext(Map(ParameterModel.AttrGroup -> "someGroup"))

    val classifier = ParameterParser.optionKeyClassifierFunc(context)
    classifier(TestKey, args, 1) should be(Some(OptionElement(TestKey, Some(TestValue))))
  }

  it should "provide a key classifier for options that handles an unknown parameter key" in {
    val UnknownKey = "unknownKey"
    val args = List("someKey", "someOtherKey", TestValue)
    val context = createModelContext(Map(ParameterModel.AttrParameterType -> ParameterModel.ParameterTypeOption))

    val classifier = ParameterParser.optionKeyClassifierFunc(context)
    classifier(UnknownKey, args, 1) should be(Some(OptionElement(UnknownKey, Some(TestValue))))
  }

  it should "provide a key classifier for options that handles a missing option value" in {
    val args = List("key1", "key2", TestKey)
    val context = createModelContext(Map(ParameterModel.AttrParameterType -> ParameterModel.ParameterTypeOption))

    val classifier = ParameterParser.optionKeyClassifierFunc(context)
    classifier(TestKey, args, 2) should be(Some(OptionElement(TestKey, None)))
  }

  it should "provide a key classifier for switches that handles a switch key" in {
    val context = createModelContext(Map(ParameterModel.AttrParameterType -> ParameterModel.ParameterTypeSwitch,
      ParameterModel.AttrSwitchValue -> "true"))

    val classifier = ParameterParser.switchKeyClassifierFunc(context)
    classifier(TestKey, Nil, 0) should be(Some(SwitchesElement(List((TestKey, "true")))))
  }

  it should "provide a key classifier for switches that evaluates the value of the switch" in {
    val context = createModelContext(Map(ParameterModel.AttrParameterType -> ParameterModel.ParameterTypeSwitch,
      ParameterModel.AttrSwitchValue -> "false"))

    val classifier = ParameterParser.switchKeyClassifierFunc(context)
    classifier(TestKey, Nil, 0) should be(Some(SwitchesElement(List((TestKey, "false")))))
  }

  it should "provide a key classifier for switches that evaluates the parameter type" in {
    val context = createModelContext(Map(ParameterModel.AttrParameterType -> ParameterModel.ParameterTypeOption))

    val classifier = ParameterParser.switchKeyClassifierFunc(context)
    classifier(TestKey, Nil, 0) should be(None)
  }

  it should "provide a key classifier for switches that deals with missing attributes" in {
    val context = createModelContext(Map.empty)

    val classifier = ParameterParser.switchKeyClassifierFunc(context)
    classifier(TestKey, Nil, 0) should be(Some(SwitchesElement(List((TestKey, "true")))))
  }
}
