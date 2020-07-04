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

import java.nio.file.Paths

import com.github.scli.ParameterExtractor.{CliExtractor, ParameterContext, ParameterExtractionException}
import com.github.scli.ParameterManager
import com.github.scli.sample.transfer.TransferParameterManager.{CryptMode, HttpServerConfig}
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import scala.util.{Failure, Success, Try}
import scala.concurrent.duration._

/**
 * Test class for ''TransferParameterManager''.
 */
class TransferParameterManagerSpec extends AnyFlatSpecLike with Matchers {
  /**
   * Parses the given command line arguments and invokes the extractor
   * specified on the result. The function expects that the extraction is
   * successful. The result is returned.
   *
   * @param args      the command line arguments
   * @param extractor the extractor to be invoked
   * @tparam T the result type of the extract
   * @return a tuple with the extraction result and the updated context
   */
  private def extract[T](args: Seq[String], extractor: CliExtractor[Try[T]]): (T, ParameterContext) = {
    val triedResult = ParameterManager.processCommandLine(args, extractor)
    triedResult match {
      case Success(tuple) => tuple
      case r => fail("Unexpected result: " + r)
    }
  }

  /**
   * Parses the given command line arguments, invokes the extractor specified
   * on the result, and returns the result produced by the extractor. The
   * extraction is supposed to be successful.
   *
   * @param args      the command line arguments
   * @param extractor the extractor to be invoked
   * @tparam T the result type of the extract
   * @return the result produced by the extractor
   */
  private def extractResult[T](args: Seq[String], extractor: CliExtractor[Try[T]]): T =
    extract(args, extractor)._1

  /**
   * Processes the given command line arguments with the extractor specified
   * and expects the operation to fail. The exception describing the failure is
   * returned.
   *
   * @param args      the command line arguments
   * @param extractor the extractor to be invoked
   * @return the exception causing the operation to fail
   */
  private def expectFailure[T](args: Seq[String], extractor: CliExtractor[Try[T]]): ParameterExtractionException =
    ParameterManager.processCommandLine(args, extractor) match {
      case Failure(e: ParameterExtractionException) => e
      case r => fail("Unexpected result: " + r)
    }

  /**
   * Processes the given command line arguments with the extractor specified
   * and expects the operation to fail at least for the option keys provided.
   * The exception describing the failure is returned.
   *
   * @param args          the command line arguments
   * @param extractor     the extractor to be invoked
   * @param failedOptions the keys of the options expected to fail
   * @return the exception causing the operation to fail
   */
  private def expectFailureInOptions[T](args: Seq[String], extractor: CliExtractor[Try[T]], failedOptions: String*):
  ParameterExtractionException = {
    val exception = expectFailure(args, extractor)
    exception.failures.map(_.key) should contain allElementsOf failedOptions
    exception
  }

  "TransferParameterManager" should "extract a TransferConfig" in {
    val args = List("upload", "--log", "log1", "file1", "--tag", "tag", "file2", "file3", "serverUri",
      "--log", "log2", "--chunk-size", "16384", "--timeout", "30")
    val ExpPaths = List(Paths.get("file1"), Paths.get("file2"), Paths.get("file3"))

    val config = extractResult(args, TransferParameterManager.transferConfigExtractor)
    config.sourceFiles should be(ExpPaths)
    config.serverUrl should be("serverUri")
    config.logs should be(List("log1", "log2"))
    config.tag should be(Some("tag"))
    config.chunkSize should be(16384)
    config.timeout should be(Some(30.seconds))
  }

  it should "set a default chunk size" in {
    val args = List("upload", "file", "serverUri")

    val config = extractResult(args, TransferParameterManager.transferConfigExtractor)
    config.chunkSize should be(TransferParameterManager.DefaultChunkSize)
  }

  it should "detect missing files to transfer in the transfer config" in {
    val args = List("upload", "serverUri")

    expectFailureInOptions(args, TransferParameterManager.transferConfigExtractor, "transferFiles")
  }

  it should "extract a CryptConfig" in {
    val args = List("--crypt-mode", "files", "--crypt-password", "secret", "--crypt-alg", "AES")

    val config = extractResult(args, TransferParameterManager.cryptConfigExtractor)
    config.cryptMode should be(CryptMode.Files)
    config.password should be("secret")
    config.algorithm should be("AES")
  }

  it should "set a default encryption algorithm" in {
    val args = List("--crypt-mode", "filesANDNames", "--crypt-password", "secret")

    val config = extractResult(args, TransferParameterManager.cryptConfigExtractor)
    config.cryptMode should be(CryptMode.FilesAndNames)
    config.algorithm should be(TransferParameterManager.DefaultCryptAlgorithm)
  }

  it should "return a dummy CryptConfig if the crypt mode is set to None" in {
    val args = List("--crypt-mode", "none")

    val config = extractResult(args, TransferParameterManager.cryptConfigExtractor)
    config should be(TransferParameterManager.DisabledCryptConfig)
  }

  it should "report a failure for an unsupported crypt mode" in {
    val args = List("--crypt-mode", "unknownCryptMode")

    expectFailureInOptions(args, TransferParameterManager.cryptConfigExtractor, "crypt-mode")
  }

  it should "set CryptMode None as default" in {
    val config = extractResult(List.empty, TransferParameterManager.cryptConfigExtractor)

    config should be(TransferParameterManager.DisabledCryptConfig)
  }

  it should "extract an HttpServerConfig" in {
    val args = List("--user", "scott", "--password", "tiger")

    val config = extractResult(args, TransferParameterManager.httpServerConfigExtractor)
    config should be(HttpServerConfig("scott", "tiger"))
  }
}
