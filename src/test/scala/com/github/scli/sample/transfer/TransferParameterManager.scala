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

import scala.concurrent.duration.Duration

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
   * An enumeration defining the usage of encryption for a transfer operation.
   *
   * With a value of this enumeration it is determined if and which data is
   * encrypted during transfer.
   */
  object CryptMode extends Enumeration {

    protected case class Val(requiresPassword: Boolean = true) extends super.Val

    /** Crypt mode indicating that encryption is disabled. */
    val None: Val = Val(requiresPassword = false)

    /** Crypt mode indicating that the content of files is encrypted. */
    val Files: Val = Val()

    /**
     * Crypt mode indicating that both the content of files and the names of
     * folders and files are encrypted.
     */
    val FilesAndNames: Val = Val()

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
   * @param password  an option with the password used for encryption
   * @param cryptMode the crypt mode; it determines whether encryption is
   *                  used
   * @param algorithm the algorithm to be used for encryption
   */
  case class CryptConfig(password: Option[String],
                         cryptMode: CryptMode.Value,
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
   * @param timeout     a timeout for I/O operations
   * @param dryRun      a flag whether only a test run should be made
   * @param logs        a list of log messages to describe the transfer
   * @param tag         an optional identifier to tag the files on the server
   */
  case class TransferConfig(sourceFiles: List[Path],
                            serverUrl: String,
                            chunkSize: Int,
                            timeout: Duration,
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

}
