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

import java.nio.file.Path
import java.util.Locale

import com.github.scli.ParameterExtractor._

import scala.concurrent.duration.Duration
import scala.util.{Success, Try}
import scala.concurrent.duration._

/**
 * An example module that provides functionality to extract command line
 * options for a hypothetical CLI application called ''transfer.''
 *
 * ''transfer'' allows uploading or downloading an arbitrary number of files to
 * or from a server. In which direction the transfer takes place, is defined by
 * a command, which is the first input parameter; it can take the values
 * ''upload'' or ''download'' respective (case is ignored). Each transfer
 * direction supports some specific flags.
 *
 * The files to be transferred and the URL of the target server are provided as
 * input parameters following the transfer direction. Depending on the server
 * type, different options need to be provided. Other options do not depend on
 * the server type and configure the transfer process in general.
 *
 * So a valid command line looks as follows:
 * {{{
 * transfer [options] <srcFile1>..<srcFileN> <serverUrl>
 * }}}
 */
object TransferParameterManager {
  /**
   * Constant for a ''CryptConfig'' that is used if encryption is disabled. In
   * this case, most properties are irrelevant.
   */
  final val DisabledCryptConfig: CryptConfig = CryptConfig(CryptMode.None, null, null)

  /** The default algorithm to be used for encryption if none is provided. */
  final val DefaultCryptAlgorithm = "RSA"

  /** The default transfer chunk size. */
  final val DefaultChunkSize = 8192

  /** The default Umask for a file server. */
  final val DefaultUmask = 775

  /** Constant for the upload transfer command. */
  final val CommandUpload = "upload"

  /** Constant for the download transfer command. */
  final val CommandDownload = "download"

  /** Conditional group identifier for a file server. */
  private val ServerTypeFile = "file"

  /** Conditional group identifier for an HTTP server. */
  private val ServerTypeHttp = "http"

  /**
   * An enumeration defining the usage of encryption for a transfer operation.
   *
   * With a value of this enumeration it is determined if and which data is
   * encrypted during transfer.
   */
  object CryptMode extends Enumeration {

    val None, Files, FilesAndNames = Value

    /**
     * A map which allows retrieving an enum value from a string constant.
     * Strings are stored in upper case.
     */
    final val Literals: Map[String, CryptMode.Value] =
      values.map(v => (v.toString.toUpperCase(Locale.ROOT), v)).toMap
  }

  /**
   * A class that combines the properties related to encryption during a
   * transfer operation.
   *
   * @param cryptMode the crypt mode; it determines whether encryption is
   *                  used
   * @param password  the password used for encryption
   * @param algorithm the algorithm to be used for encryption
   */
  case class CryptConfig(cryptMode: CryptMode.Value,
                         password: String,
                         algorithm: String)

  /**
   * A class for storing some general properties for a transfer operation.
   *
   * These properties are common to all supported commands and do not fit into
   * a specific category.
   *
   * @param sourceFiles a list with the paths to the files to be transferred
   * @param serverUrl   the URL of the target server
   * @param chunkSize   the chunks to use for I/O operations
   * @param timeout     an optional timeout for I/O operations
   * @param dryRun      a flag whether only a test run should be made
   * @param logs        a list of log messages to describe the transfer
   * @param tag         an optional identifier to tag the files on the server
   */
  case class TransferConfig(sourceFiles: List[Path],
                            serverUrl: String,
                            chunkSize: Int,
                            timeout: Option[Duration],
                            dryRun: Boolean,
                            logs: Iterable[String],
                            tag: Option[String])

  /**
   * A trait to represent the command line options to extract depending on the
   * command passed as the first argument to the application.
   */
  sealed trait CommandConfig

  /**
   * The options supported by the transfer upload command.
   *
   * @param uploadHashes        flag whether file hashes should be generated and
   *                            uploaded
   * @param removeUploadedFiles flag whether local files should be removed
   *                            after a successful upload
   */
  case class UploadCommandConfig(uploadHashes: Boolean,
                                 removeUploadedFiles: Boolean) extends CommandConfig

  /**
   * The options supported by the transfer download command.
   *
   * @param targetFolder       the folder where to store downloaded files
   * @param overrideLocalFiles flag whether existing files should be overridden
   */
  case class DownloadCommandConfig(targetFolder: Path,
                                   overrideLocalFiles: Boolean) extends CommandConfig

  /**
   * A trait to represent the command line options to extract based on the type
   * of the server URI.
   *
   * The server URI is evaluated, and based on its scheme, one of the classes
   * extending this trait is instantiated and populated.
   */
  sealed trait ServerConfig

  /**
   * Command line options to be applied if the target server is a file server.
   *
   * @param rootPath optional root path where to store files
   * @param umask    defines the access rights for files to be stored
   */
  case class FileServerConfig(rootPath: Option[String],
                              umask: Int) extends ServerConfig

  /**
   * Command line options to be applied if the target server is an HTTP server.
   *
   * @param user     the user name for authentication
   * @param password the password for authentication
   */
  case class HttpServerConfig(user: String,
                              password: String) extends ServerConfig

  /**
   * A data class that combines all the command line options that control a
   * transfer operation.
   *
   * An instance of this class is the result of the CLI processing. Based on
   * the variable command and server configurations, the exact transfer process
   * to initiate can be determined.
   *
   * @param commandConfig  the config related to the command
   * @param serverConfig   the config related to the target server
   * @param cryptConfig    the config related to encryption
   * @param transferConfig the general transfer config
   */
  case class TransferCommandConfig(commandConfig: CommandConfig,
                                   serverConfig: ServerConfig,
                                   cryptConfig: CryptConfig,
                                   transferConfig: TransferConfig)

  /**
   * Returns an extractor for the configuration for encryption. Whether
   * encryption is enabled (and additional properties must be provided),
   * depends on the crypt mode.
   *
   * @return the extractor for the ''CryptConfig''
   */
  def cryptConfigExtractor: CliExtractor[Try[CryptConfig]] = {
    val extCryptEnabled = cryptModeExtractor
      .map(triedMode => triedMode.map(_ != CryptMode.None))
    conditionalValue(extCryptEnabled, ifExt = definedCryptConfigExtractor,
      elseExt = constantExtractor(Success(DisabledCryptConfig)))
  }

  /**
   * Returns an extractor for the configuration of an HTTP server. All
   * properties of this configuration are mandatory; the extractor is applied
   * only if the target server is actually an HTTP server.
   *
   * @return the extractor for the ''HttpServerConfig''
   */
  def httpServerConfigExtractor: CliExtractor[Try[ServerConfig]] = {
    val extUsr = optionValue("user")
      .mandatory
    val extPwd = passwordExtractor("password", "HTTP server password")
    for {
      user <- extUsr
      pwd <- extPwd
    } yield createRepresentation(user, pwd)(HttpServerConfig)
  }

  /**
   * Returns an extractor for the ''TransferConfig''.
   *
   * @return the extractor for the ''TransferConfig''
   */
  def transferConfigExtractor: CliExtractor[Try[TransferConfig]] = {
    val extSrcFiles = inputValues(fromIdx = 1, toIdx = -2, optKey = Some("transferFiles"))
      .multiplicity(atLeast = 1)
      .toPath
      .map(_.map(_.toList))
    val extServerUri = serverUriExtractor.mandatory
    val extChunkSize = optionValue("chunk-size")
      .toInt
      .fallbackValue(DefaultChunkSize)
      .mandatory
    val extTimeout = optionValue("timeout")
      .toInt
      .mapTo(t => t.seconds)
    for {
      srcFiles <- extSrcFiles
      serverUri <- extServerUri
      logs <- multiOptionValue("log")
      tag <- optionValue("tag")
      chunkSize <- extChunkSize
      timeout <- extTimeout
    } yield createRepresentation(srcFiles, serverUri, chunkSize, timeout, logs, tag) {
      TransferConfig(_, _, _, _, false, _, _)
    }
  }

  /**
   * Returns an extractor for the ''TransferCommandConfig''. This is the
   * top-level extractor which combines all other extractors to process the
   * full command line.
   *
   * @return the extractor for the ''TransferCommandConfig''
   */
  def transferCommandConfigExtractor: CliExtractor[Try[TransferCommandConfig]] = {
    for {
      cmd <- commandConfigExtractor
      svr <- serverConfigExtractor
      crypt <- cryptConfigExtractor
      trans <- transferConfigExtractor
    } yield createRepresentation(cmd, svr, crypt, trans)(TransferCommandConfig)
  }

  /**
   * Returns the extractor for the command config. Depending on the transfer
   * command passed as the first input parameter, either the extractor for
   * the ''UploadCommandConfig'' or the ''DownloadCommandConfig'' needs to be
   * run.
   *
   * @return the extractor for the command config
   */
  private def commandConfigExtractor: CliExtractor[Try[CommandConfig]] = {
    val extCmdName = inputValue(index = 0, optKey = Some("transfer-command"))
      .toLower
      .mandatory
    val groupExtractors = Map(CommandUpload -> uploadCommandConfigExtractor,
      CommandDownload -> downloadCommandConfigExtractor)
    conditionalGroupValue(extCmdName, groupExtractors)
  }

  /**
   * Returns the extractor for the server configuration. Based on the server
   * URI, its type is determined. Depending on the type then a specific
   * server configuration is extracted.
   *
   * @return the extractor for the server config
   */
  private def serverConfigExtractor: CliExtractor[Try[ServerConfig]] = {
    val groupExtractors = Map(ServerTypeFile -> fileServerConfigExtractor,
      ServerTypeHttp -> httpServerConfigExtractor)
    conditionalGroupValue(serverTypeExtractor, groupExtractors)
  }

  /**
   * Returns an extractor to determine the server type based on its URI. This
   * type determines, which additional options are supported to configure the
   * server.
   *
   * @return the extractor for the server type
   */
  private def serverTypeExtractor: CliExtractor[Try[String]] =
    serverUriExtractor.mapTo { uri =>
      if (uri.startsWith("http://") || uri.startsWith("https://")) ServerTypeHttp else ServerTypeFile
    }.mandatory

  /**
   * Returns the extractor for the URI of the target server. The URI is read
   * from the last input parameter.
   *
   * @return the extractor for the URI of the server
   */
  private def serverUriExtractor: CliExtractor[SingleOptionValue[String]] =
    inputValue(optKey = Some("serverUri"), index = -1)

  /**
   * Returns an extractor for a (mandatory) password option. The extractor
   * reads the password from the console if it has not been specified on the
   * command line.
   *
   * @param key    the key of the option
   * @param prompt the string to prompt the user
   * @return the password extractor for this key
   */
  private def passwordExtractor(key: String, prompt: String): CliExtractor[Try[String]] =
    optionValue(key)
      .fallback(consoleReaderValue(key, optPrompt = Some(prompt)))
      .mandatory

  /**
   * Returns an extractor that extracts a crypt mode value from a command line
   * option.
   *
   * @return the extractor to extract the crypt mode
   */
  private def cryptModeExtractor: CliExtractor[Try[CryptMode.Value]] =
    optionValue("crypt-mode")
      .toUpper
      .toEnum(CryptMode.Literals.get)
      .fallbackValue(CryptMode.None)
      .mandatory

  /**
   * Returns an extractor for the ''CryptConfig'' if the crypt mode is set to
   * something different than ''None''. Only then additional options are
   * evaluated.
   *
   * @return the extractor for the defined ''CryptConfig''
   */
  private def definedCryptConfigExtractor: CliExtractor[Try[CryptConfig]] = {
    val extCryptPass = passwordExtractor("crypt-password", "Encryption password")
    val extCryptAlg = optionValue("crypt-alg")
      .fallbackValue(DefaultCryptAlgorithm)
      .mandatory
    for {
      mode <- cryptModeExtractor
      pwd <- extCryptPass
      alg <- extCryptAlg
    } yield createRepresentation(mode, pwd, alg)(CryptConfig)
  }

  /**
   * Returns an extractor for the ''UploadCommandConfig''.
   *
   * @return the extractor for the ''UploadCommandConfig''
   */
  private def uploadCommandConfigExtractor: CliExtractor[Try[CommandConfig]] = {
    val extUploadHashes = optionValue("upload-hashes")
      .toBoolean
      .fallbackValue(false)
      .mandatory
    val extRemoveUploaded = optionValue("remove-uploaded-files")
      .toBoolean
      .fallbackValue(false)
      .mandatory
    for {
      uploadHashes <- extUploadHashes
      removeUploaded <- extRemoveUploaded
    } yield createRepresentation(uploadHashes, removeUploaded)(UploadCommandConfig)
  }

  /**
   * Returns an extractor for the configuration of the download command.
   *
   * @return the extractor for the ''DownloadCommandConfig''
   */
  private def downloadCommandConfigExtractor: CliExtractor[Try[CommandConfig]] = {
    val extTargetPath = optionValue("target-folder")
      .toPath
      .mandatory
    val extOverride = optionValue("override-local-files")
      .toBoolean
      .fallbackValue(false)
      .mandatory
    for {
      target <- extTargetPath
      fOverride <- extOverride
    } yield createRepresentation(target, fOverride)(DownloadCommandConfig)
  }

  /**
   * Returns an extractor for the configuration of a file server.
   *
   * @return the extractor for the ''FileServerConfig''
   */
  private def fileServerConfigExtractor: CliExtractor[Try[ServerConfig]] = {
    val extRootPath = optionValue("root-path")
    val extUmask = optionValue("umask")
      .toInt
      .fallbackValue(DefaultUmask)
      .mandatory
    for {
      rootPath <- extRootPath
      mask <- extUmask
    } yield createRepresentation(rootPath, mask)(FileServerConfig)
  }
}
