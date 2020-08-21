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

package com.github.scli.sample.transfer

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.util.regex.Pattern

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ArrayBuffer

object TransferAppSpec {
  /** URI of a test HTTP server. */
  private val HttpServerUri = "https://webdav.transfer.org/data"

  /** URI of a test file server. */
  private val FileServerUri = "/mount/shared/drive/folder"

  /**
   * Generates the name of a transfer file based on the given index.
   *
   * @param index the index of the test transfer file
   * @return the name of this file
   */
  private def transferFile(index: Int): String = s"transferFile$index.zip"

  /**
   * Generates the server URI based on the HTTP flag.
   *
   * @param httpServer flag whether an HTTP server should be used
   * @return the server URI
   */
  private def serverUri(httpServer: Boolean): String =
    if (httpServer) HttpServerUri else FileServerUri

  /**
   * Generates an array with command line parameters containing input
   * parameters matching the criteria provided plus the given arguments.
   *
   * @param command    the transfer command
   * @param fileCount  th number of transfer files
   * @param httpServer flag whether an HTTP server should be used
   * @param args       additional arguments
   * @return the resulting array with command line arguments
   */
  private def withInputParameters(command: String, fileCount: Int, httpServer: Boolean, args: String*): Array[String] = {
    val files = (1 to fileCount) map transferFile
    val uri = serverUri(httpServer)
    val buffer = ArrayBuffer(command)
    buffer ++= files
    buffer += uri
    buffer ++= args
    buffer.toArray
  }

  /**
   * Executes the transfer application with the parameters specified and
   * captures the output produced by the application. This output is returned.
   *
   * @param args the command line arguments for the transfer application
   * @return the output printed by the application
   */
  private def executeTransfer(args: Array[String]): String = {
    val stdOut = new ByteArrayOutputStream
    Console.withOut(stdOut) {
      TransferApp.main(args)
    }
    stdOut.toString(StandardCharsets.UTF_8)
  }
}

/**
 * Test class for ''TransferApp''. This class executes the transfer application
 * with test command lines and checks whether it produces the expected output.
 */
class TransferAppSpec extends AnyFlatSpec with Matchers {

  import TransferAppSpec._

  /**
   * Checks whether the given output indicates a transfer operation with the
   * parameters specified.
   *
   * @param output     the output to check
   * @param upload     flag whether this was an upload operation
   * @param fileCount  the number of files to be transferred
   * @param httpServer flag whether an HTTP server should be used
   * @return the output string
   */
  private def checkTransfer(output: String, upload: Boolean, fileCount: Int, httpServer: Boolean): String = {
    val expOpString = if (upload) "Uploaded" else "Downloaded"
    val expFiles = (1 to fileCount).map(transferFile).mkString(", ")
    output should include("Executing transfer operation.")
    output should include(expOpString)
    output should include(expFiles)
    output should include(serverUri(httpServer))
    output
  }

  /**
   * Checks whether the given output indicates that help information is
   * displayed.
   *
   * @param output the output to check
   * @return the output string
   */
  private def checkHelp(output: String): String = {
    output should include("Usage: transfer [options]")
    output
  }

  /**
   * Checks whether the given output contains a help text for the key
   * specified. Via a regular expression, it is checked whether the key and the
   * help text (which can be a fragment) appear on the same line in the output.
   *
   * @param output the output to check
   * @param key    the key in question
   * @param help   a help text fragment for this key
   * @return the position where the text was found
   */
  private def assertHelpForKey(output: String, key: String, help: String): Int = {
    val reg = (Pattern.quote(key) + ".+" + Pattern.quote(help)).r
    val optMatch = reg.findFirstMatchIn(output)
    optMatch.isDefined shouldBe true
    optMatch.get.start
  }

  "TransferApp" should "execute a transfer upload operation for valid parameters" in {
    val args = withInputParameters("upload", 2, httpServer = false)
    checkTransfer(executeTransfer(args), upload = true, fileCount = 2, httpServer = false)
  }

  it should "execute a transfer download operation for valid parameters" in {
    val User = "scott"
    val Password = "tiger"
    val Target = "/data/downloads"
    val args = withInputParameters("Download", fileCount = 4, httpServer = true,
      "--user", User, "--password", Password, "--target-folder", Target)

    val output = checkTransfer(executeTransfer(args), upload = false, fileCount = 4, httpServer = true)
    output should include(User)
    output should include(Target)
  }

  it should "display an overview of input parameters if the command line is invalid" in {
    val output = checkHelp(executeTransfer(Array.empty))

    output should include("<transferCommand>")
    output should include("<transferFiles>")
    output should include("<serverUri>")
  }

  it should "display help for input parameters if the command line is invalid" in {
    val output = checkHelp(executeTransfer(Array.empty))

    assertHelpForKey(output, "transferCommand", "The command defining")
    assertHelpForKey(output, "transferFiles", "A list of files to")
    assertHelpForKey(output, "serverUri", "The URI of the server")
  }

  it should "display help for options and switches if the command line is invalid" in {
    val output = checkHelp(executeTransfer(Array.empty))

    assertHelpForKey(output, "chunk-size", "Defines the chunk size")
    assertHelpForKey(output, "dry-run", "Allows enabling a dry-run")
  }

  it should "display input parameters before options in the help text" in {
    val args = withInputParameters("upload", 1, httpServer = false, "--unknown")
    val output = checkHelp(executeTransfer(args))

    val posOption = assertHelpForKey(output, "chunk-size", "Defines the chunk size")
    val posParam = assertHelpForKey(output, "transferFiles", "A list of files to")
    posParam should be < posOption
  }
}
