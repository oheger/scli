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

import java.nio.file.{Files, Paths}

import com.github.scli.ParameterExtractor.ParameterExtractionException
import com.github.scli.ParameterManager.ProcessingContext
import com.github.scli.ParametersTestHelper._
import com.github.scli.sample.transfer.TransferParameterManager.{CryptMode, _}
import com.github.scli.{ConsoleReader, ParameterExtractor}
import org.mockito.Mockito
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.mockito.MockitoSugar

import scala.concurrent.duration._
import scala.util.{Failure, Success}

/**
 * Test class for ''TransferParameterManager''.
 */
class TransferParameterManagerSpec extends AnyFlatSpecLike with Matchers with MockitoSugar {
  /**
   * Returns a list with default input parameters. These parameters are needed
   * by many tests to have a valid command line.
   *
   * @param http flag whether an HTTP server URI should be used
   * @return the default input parameters
   */
  private def inputParameters(http: Boolean): List[String] =
    List("upload", "file", if (http) "https://server.org" else "/file/server")

  /**
   * Parses the given command line arguments and invokes the extractor
   * specified on the result. The function expects that the extraction is
   * successful. The result is returned.
   *
   * @param args the command line arguments
   * @return a tuple with the extraction result and the updated context
   */
  private def extract(args: Seq[String]): (TransferParameterManager.TransferCommandConfig, ProcessingContext) = {
    val triedResult = TransferParameterManager.processCommandLine(args)
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
   * @param args the command line arguments
   * @return the result produced by the extractor
   */
  private def extractResult(args: Seq[String]): TransferParameterManager.TransferCommandConfig =
    extract(args)._1

  /**
   * Processes the given command line arguments with the extractor specified
   * and expects the operation to fail. The exception describing the failure is
   * returned.
   *
   * @param args the command line arguments
   * @return the exception causing the operation to fail
   */
  private def expectFailure(args: Seq[String]): ParameterExtractionException =
    TransferParameterManager.processCommandLine(args) match {
      case Failure(e: ParameterExtractionException) => e
      case r => fail("Unexpected result: " + r)
    }

  /**
   * Processes the given command line arguments with the extractor specified
   * and expects the operation to fail at least for the option keys provided.
   * The exception describing the failure is returned.
   *
   * @param args          the command line arguments
   * @param failedOptions the keys of the options expected to fail
   * @return the exception causing the operation to fail
   */
  private def expectFailureInOptions[T](args: Seq[String], failedOptions: String*): ParameterExtractionException = {
    val exception = expectFailure(args)
    exception.failures.map(_.key.key) should contain allElementsOf failedOptions
    exception
  }

  "TransferParameterManager" should "extract a TransferConfig" in {
    val args = List("upload", "--log", "log1", "file1", "--tag", "tag", "file2", "file3", "serverUri",
      "--log", "log2", "--chunk-size", "16384", "--timeout", "30", "--dry-run")
    val ExpPaths = List(Paths.get("file1"), Paths.get("file2"), Paths.get("file3"))

    val config = extractResult(args).transferConfig
    config.sourceFiles should be(ExpPaths)
    config.serverUrl should be("serverUri")
    config.logs should be(List("log1", "log2"))
    config.tag should be(Some("tag"))
    config.chunkSize should be(16384)
    config.timeout should be(Some(30.seconds))
    config.dryRun shouldBe true
  }

  it should "set a default chunk size and dry-run flag" in {
    val args = List("upload", "file", "serverUri")

    val config = extractResult(args).transferConfig
    config.chunkSize should be(TransferParameterManager.DefaultChunkSize)
    config.dryRun shouldBe false
  }

  it should "define aliases for some parameters of the TransferConfig" in {
    val args = inputParameters(http = false) ::: List("-l", "log1", "-T", "tag",
      "--log", "log2", "-s", "16384", "-t", "30", "-d")

    val config = extractResult(args).transferConfig
    config.logs should be(List("log1", "log2"))
    config.tag should be(Some("tag"))
    config.chunkSize should be(16384)
    config.timeout should be(Some(30.seconds))
    config.dryRun shouldBe true
  }

  it should "detect missing files to transfer in the transfer config" in {
    val args = List("upload", "serverUri")

    expectFailureInOptions(args, "transferFiles")
  }

  it should "fail for an unexpected parameter" in {
    val UnexpectedParam = "unsupported"
    val args = List("upload", "file1", "serverUri", "--tag", "test", "--" + UnexpectedParam, "strange")

    expectFailureInOptions(args, UnexpectedParam)
  }

  it should "extract a CryptConfig" in {
    val args = inputParameters(false) ::: List("--crypt-mode", "files", "--crypt-password", "secret",
      "--crypt-alg", "AES")

    val config = extractResult(args).cryptConfig
    config.cryptMode should be(CryptMode.Files)
    config.password should be("secret")
    config.algorithm should be("AES")
  }

  it should "set a default encryption algorithm" in {
    val args = inputParameters(false) ::: List("-c", "filesANDNames", "--crypt-password", "secret")

    val config = extractResult(args).cryptConfig
    config.cryptMode should be(CryptMode.FilesAndNames)
    config.algorithm should be(TransferParameterManager.DefaultCryptAlgorithm)
  }

  it should "return a dummy CryptConfig if the crypt mode is set to None" in {
    val args = inputParameters(false) ::: List("--crypt-mode", "none")

    val config = extractResult(args)
    config.cryptConfig should be(TransferParameterManager.DisabledCryptConfig)
  }

  it should "report a failure for an unsupported crypt mode" in {
    val args = inputParameters(false) ::: List("--crypt-mode", "unknownCryptMode")

    expectFailureInOptions(args, "crypt-mode")
  }

  it should "set CryptMode None as default" in {
    val config = extractResult(inputParameters(false))

    config.cryptConfig should be(TransferParameterManager.DisabledCryptConfig)
  }

  it should "read the encryption password from the console if it is not specified" in {
    val args = Map("crypt-mode" -> List("files"),
      "crypt-alg" -> List("DES"))
    val Password = "secret_Encryption!Pwd"
    implicit val consoleReader: ConsoleReader = mock[ConsoleReader]
    Mockito.when(consoleReader.readOption("Encryption password", password = true)).thenReturn(Password)

    val (result, _) = ParameterExtractor.runExtractor(TransferParameterManager.cryptConfigExtractor, args)
    result.map(_.password) should be(Success(Password))
  }

  it should "extract an HttpServerConfig" in {
    val args = inputParameters(http = true) ::: List("--user", "scott", "--password", "tiger")

    val config = extractResult(args)
    config.serverConfig should be(HttpServerConfig("scott", "tiger"))
  }

  it should "read the HTTP server password from the console if it is not specified" in {
    val args = Map("user" -> List("scott"))
    val Password = "tiger"
    implicit val consoleReader: ConsoleReader = mock[ConsoleReader]
    Mockito.when(consoleReader.readOption("HTTP server password", password = true)).thenReturn(Password)

    val (result, _) = ParameterExtractor.runExtractor(TransferParameterManager.httpServerConfigExtractor, args)
    result match {
      case Success(config: HttpServerConfig) =>
        config.password should be(Password)
      case r => fail("Unexpected result: " + r)
    }
  }

  it should "extract a command config for uploads and a file server" in {
    val args = List("upload", "file1", "file2", "/file/server", "--upload-hashes",
      "--remove-uploaded-files", "--root-path", "/data", "--umask", "660",
      "--crypt-mode", "filesANDNames", "--crypt-password", "secret")

    val config = extractResult(args)
    config.cryptConfig should be(CryptConfig(CryptMode.FilesAndNames, "secret",
      TransferParameterManager.DefaultCryptAlgorithm))
    config.transferConfig.serverUrl should be("/file/server")
    config.transferConfig.sourceFiles should contain only(Paths get "file1", Paths get "file2")
    config.commandConfig should be(UploadCommandConfig(uploadHashes = true, removeUploadedFiles = true))
    config.serverConfig should be(FileServerConfig(Some("/data"), 660))
  }

  it should "set default values for the upload config" in {
    val args = List("upload", "file1", "file2", "/file/server", "--root-path", "/data", "--umask", "660")

    val config = extractResult(args)
    config.commandConfig should be(UploadCommandConfig(uploadHashes = false, removeUploadedFiles = false))
  }

  it should "set default values for the file server config" in {
    val args = List("upload", "file1", "file2", "/file/server", "--upload-hashes")

    val config = extractResult(args)
    config.serverConfig should be(FileServerConfig(rootPath = None, umask = TransferParameterManager.DefaultUmask))
  }

  it should "extract a command config for downloads and a file server" in {
    val args = List("download", "file1", "file2", "/file/server", "--target-folder", "target",
      "--skip-existing", "--root-path", "/data", "--umask", "660",
      "--crypt-mode", "filesANDNames", "--crypt-password", "secret")

    val config = extractResult(args)
    config.cryptConfig should be(CryptConfig(CryptMode.FilesAndNames, "secret",
      TransferParameterManager.DefaultCryptAlgorithm))
    config.transferConfig.serverUrl should be("/file/server")
    config.transferConfig.sourceFiles should contain only(Paths get "file1", Paths get "file2")
    config.commandConfig should be(DownloadCommandConfig(targetFolder = Paths get "target", overrideLocalFiles = false))
    config.serverConfig should be(FileServerConfig(Some("/data"), 660))
  }

  it should "set default values for the download config" in {
    val args = List("download", "file1", "file2", "/file/server", "--target-folder", "target")

    val config = extractResult(args)
    config.cryptConfig should be(TransferParameterManager.DisabledCryptConfig)
    config.commandConfig should be(DownloadCommandConfig(targetFolder = Paths get "target", overrideLocalFiles = true))
  }

  it should "detect an unsupported transfer command" in {
    val args = List("unknownCmd", "file1", "file2", "/file/server", "--target-folder", "target")

    expectFailureInOptions(args, "transfer-command")
  }

  it should "detect transfer commands in a case-insensitive manner" in {
    val args = List("DownLoad", "file1", "file2", "/file/server", "--target-folder", "target")

    val config = extractResult(args)
    config.commandConfig should be(DownloadCommandConfig(targetFolder = Paths get "target", overrideLocalFiles = true))
  }

  it should "extract a command config for downloads and an HTTP server" in {
    val ServerUri = "http://www.test-server.org"
    val args = List("download", "file1", "file2", ServerUri, "--user", "scott", "--password", "tiger",
      "--target-folder", "target")

    val config = extractResult(args)
    config.serverConfig should be(HttpServerConfig("scott", "tiger"))
    config.commandConfig should be(DownloadCommandConfig(targetFolder = Paths get "target", overrideLocalFiles = true))
  }

  it should "extract a command config for uploads and an HTTP server" in {
    val ServerUri = "https://www.test-server.org"
    val args = List("upload", "file1", "file2", ServerUri, "--user", "scott", "--password", "tiger",
      "--upload-hashes")

    val config = extractResult(args)
    config.serverConfig should be(HttpServerConfig("scott", "tiger"))
    config.commandConfig should be(UploadCommandConfig(uploadHashes = true, removeUploadedFiles = false))
  }

  it should "detect unexpected parameters from a different server type" in {
    val args = List("upload", "file1", "file2", "/file/server", "--user", "scott", "--password", "tiger")

    expectFailureInOptions(args, "user", "password")
  }

  it should "treat long options in a case-insensitive way" in {
    val args = inputParameters(http = false) ::: List("--Log", "log1", "--TAG", "tag",
      "--Chunk-Size", "16384", "--TIMEOUT", "30", "--Dry-RUN")

    val config = extractResult(args).transferConfig
    config.logs should be(List("log1"))
    config.tag should be(Some("tag"))
    config.chunkSize should be(16384)
    config.timeout should be(Some(30.seconds))
    config.dryRun shouldBe true
  }

  /**
   * Processes a command line that references a file and checks whether this
   * file is correctly evaluated.
   *
   * @param key the key of the option referencing the file
   */
  private def checkSupportForParameterFiles(key: String): Unit = {
    val serverConfig = HttpServerConfig("scott", "tiger")
    val fileContent = List("--timeout", "15", "--chunk-size", "8192", "--user", serverConfig.user,
      "--password", serverConfig.password)
    import scala.jdk.CollectionConverters._
    val path = Files.createTempFile("scliTest", ".tmp")
    val paramFile = Files.write(path, fileContent.asJava)
    try {
      val args = inputParameters(http = true) ::: List(key, paramFile.toAbsolutePath.toString)
      val config = extractResult(args)
      config.transferConfig.chunkSize should be(8192)
      config.transferConfig.timeout should be(Some(15.seconds))
      config.serverConfig should be(serverConfig)
    } finally {
      Files.delete(paramFile)
    }
  }

  it should "support parameter files" in {
    checkSupportForParameterFiles("--param-file")
  }

  it should "support parameter files with a short alias" in {
    checkSupportForParameterFiles("-f")
  }

  it should "support combining switches in a single parameter" in {
    val args = inputParameters(http = false) ::: List("--chunk-size", "8192", "-dHC")
    val expUploadConfig = UploadCommandConfig(uploadHashes = true, removeUploadedFiles = true)

    val config = extractResult(args)
    config.transferConfig.dryRun shouldBe true
    config.commandConfig should be(expUploadConfig)
  }
}
