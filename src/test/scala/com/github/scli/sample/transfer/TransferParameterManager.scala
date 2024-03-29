/*
 * Copyright 2020-2021 The Developers Team.
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

import com.github.scli.HelpGenerator.ParameterFilter
import com.github.scli.ParameterExtractor._
import com.github.scli.ParameterManager.{ExtractionSpec, ProcessingContext}
import com.github.scli.ParameterModel.{AttrErrMessage, AttrFallbackValue, AttrHelpText, ParameterKey}
import com.github.scli.{HelpGenerator, ParameterManager}

import scala.concurrent.duration.{Duration, _}
import scala.util.{Success, Try}

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

  /** A group for parameters related to encryption. */
  private val GroupEncryption = "encryption"

  private val HelpTransferCommand =
    """The command defining the transfer operation to be executed. Depending on the command, additional \
      |command-specific parameters are enabled or disabled. The following commands are supported \
      |(case does not matter):
      |- upload: uploads files to the server
      |- download: downloads files from the server""".stripMargin

  private val HelpTransferFiles =
    """A list of files to be uploaded to or downloaded from the target server."""

  private val HelpTransferServer =
    """The URI of the server which is the target of the transfer operation. Different types of servers \
      |are supported; depending on the server type, further parameters are enabled or disabled. \
      |The server type is determined by the scheme of the URI provided: the schemes 'http' \
      |or 'https' select an HTTP server; for all other schemes a file server is used.""".stripMargin

  private val HelpChunkSize =
    """Defines the chunk size for transfer operations (in kilobytes). Using this option, the data transfer \
      |can be tweaked towards smaller or larger files.""".stripMargin

  private val HelpTimeout =
    """Defines a timeout (in seconds) for transfer operations. If a transfer takes longer than this time, \
      |it is canceled, and an error is recorded.""".stripMargin

  private val HelpLog =
    """Allows specifying log messages for the current transfer operation. The parameter can occur multiple \
      |times, so that multiple log messages can be set.""".stripMargin

  private val HelpTag =
    """Allows specifying a tag for the current transfer operation. The tag is stored in the transfer log."""

  private val HelpDryRun =
    """Allows enabling a dry-run or test mode, in which no actual files are transferred."""

  private val HelpAttribute =
    """Defines an attribute for the current transfer operation, which is stored in the metadata. \
      |The value of this option must be in the form <key>=<value>. By repeating this parameter,
      |multiple attributes can be set.""".stripMargin

  private val HelpCryptMode =
    """Determines what kind of encryption is used during the transfer process. This parameter \
      |can have the following values (case does not matter):
      |- None: encryption is disabled
      |- Files: the content of files is encrypted
      |- FilesAndNames: the content of files and their names are encrypted""".stripMargin

  private val HelpCryptPassword =
    """Sets the password to be used for encryption. This parameter is evaluated only if encryption \
      |is enabled; then it must be either provided on the command line or it is read from the console.""".stripMargin

  private val HelpCryptAlg =
    """Defines the algorithm to be used for encryption. This parameter is evaluated only if encryption \
      |is enabled.""".stripMargin

  private val HelpHttpServerUser =
    """Sets the user name for authentication against an HTTP server."""

  private val HelpHttpServerPassword =
    """Sets the password for authentication against an HTTP server. This parameter is evaluated only \
      |if the target server is an HTTP server; then the password must either be provided on the command \
      |line or it is read from the console.""".stripMargin

  private val HelpFileServerRoot =
    """Sets the root path of the file server. All files that are transferred are stored in this path."""

  private val HelpFileServerUmask =
    """Defines the Unix umask for new files stored on the file server."""

  private val HelpUploadHashes =
    """Determines whether hashes should be uploaded together with files."""

  private val HelpRemoveUploaded =
    """Determines whether local files should be removed after a successful upload. (If an upload fails, \
      |the file is not removed.)""".stripMargin

  private val HelpTargetFolder =
    """Defines the local target folder for download operations. All files that are downloaded are stored \
      |in a folder structure below this directory.""".stripMargin

  private val HelpSkipExisting =
    """Determines whether files already existing on the local hard drive should not be overridden by \
      |files downloaded from the server.""".stripMargin

  private val HelpLogDebug =
    """Sets the log level to 'DEBUG'. This causes the most verbose output with lots of diagnostic information."""

  private val HelpLogInfo =
    """Sets the log level to 'INFO'. This causes some important information to be printed during a transfer \
      |operation.""".stripMargin

  private val HelpLogWarn =
    """Sets the log level to 'WARN'. In this mode, only warnings and errors are displayed during a transfer \
      |operation.""".stripMargin

  private val HelpLogError =
    """Sets the log level to 'ERROR'. This mode produces the least output; only errors occurring during a transfer \
      |operation are reported.""".stripMargin

  private val HelpHelp =
    """Displays a screen with help information.
      |transfer --help shows information about the parameters common to all commands.
      |transfer <command> --help in addition shows the parameters specific to this command.
      |Some parameters are specific to the target server of transfer operations. In order to display \
      |them, a valid server URI must be provided, e.g.:
      |transfer download file.txt http://target.server.com --help
      |To include help about options related to encryption, specify a valid crypt mode, e.g.:
      |transfer --crypt-mode files --help""".stripMargin

  /** Regular expression to match attribute parameters. */
  private val RegAttribute = "(.+)=(.+)".r

  private[transfer] val ErrorHeader = "Invalid parameters have been detected:"

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
   * A data class representing a metadata attribute for a transfer operation.
   *
   * It is possible to specify an arbitrary number of key-value pairs that are
   * stored as attributes for the current transfer operation.
   *
   * @param key   the key of the attribute
   * @param value the value of the attribute
   */
  case class Attribute(key: String, value: String)

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
   * @param attributes  a number of attributes describing the transfer
   * @param logLevel    the log level for generating log messages
   */
  case class TransferConfig(sourceFiles: List[Path],
                            serverUrl: String,
                            chunkSize: Int,
                            timeout: Option[Duration],
                            dryRun: Boolean,
                            logs: Iterable[String],
                            tag: Option[String],
                            attributes: Iterable[Attribute],
                            logLevel: String)

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

  /** The top-level extractor for the ''TransferCommandConfig''. */
  private val transferCommandConfigExtractor: CliExtractor[Try[TransferCommandConfig]] =
    createTransferCommandConfigExtractor()

  /**
   * Processes the given sequence of command line arguments and returns a
   * ''Try'' with the results. This function has to be called with the
   * arguments passed to the application. If successful, from the result the
   * ''TransferCommandConfig'' with all settings provided by the user can be
   * extracted.
   *
   * @param args the sequence with command line arguments
   * @return a ''Try'' with the results of command line processing
   */
  def processCommandLine(args: Seq[String]): Try[(TransferCommandConfig, ProcessingContext)] = {
    val keyExtractor = ParameterManager.defaultKeyExtractor() andThen (opt =>
      opt.map(key => if (key.shortAlias) key else key.copy(key = key.key.toLowerCase(Locale.ROOT))))
    val helpSwitch = switchValue("help", optHelp = Some(HelpHelp))
      .alias("h")
    val spec = ExtractionSpec(transferCommandConfigExtractor, keyExtractor = keyExtractor,
      supportCombinedSwitches = true, optHelpExtractor = Some(helpSwitch),
      fileOptions = List(ParameterKey("param-file", shortAlias = false), ParameterKey("f", shortAlias = true)),
      exceptionGenerator = exceptionGenerator,
      optExceptionMapper = Some(exceptionMapper))
    val classifierFunc = ParameterManager.classifierFunc(spec)
    val parseFunc = ParameterManager.parsingFuncForClassifier(spec)(classifierFunc)

    for {
      processedArgs <- ParameterManager.processParameterFiles(args, spec)(classifierFunc)
      result <- ParameterManager.processCommandLineSpec(processedArgs, spec, parser = parseFunc)
    } yield result
  }

  /**
   * Processes the given sequence of command line arguments and evaluates the
   * result. The resulting ''Either'' can be used to determine whether
   * processing should continue or help information should be displayed.
   *
   * @param args the sequence with command line arguments
   * @return an ''Either'' with the evaluated command line processing result
   */
  def evaluateCommandLine(args: Seq[String]): Either[ProcessingContext, TransferCommandConfig] =
    ParameterManager.evaluate(processCommandLine(args))

  /**
   * Generates help information for this application based on the context
   * provided.
   *
   * @param context the ''ProcessingContext'' with the current parameters and
   *                errors
   * @return a formatted string with help information
   */
  def generateHelp(context: ProcessingContext): String = {
    import HelpGenerator._
    val modelContext = context.parameterContext.modelContext
    val paramNameGenerator = parameterNameColumnGenerator()
    val optionKeyGenerator = suffixGeneratedColumnGenerator(paramNameGenerator,
      mandatoryColumnGenerator(optMandatoryText = Some("*")))
    val keyGenerator = parameterKeyGeneratedWithAliasesColumnGenerator(optionKeyGenerator, maxLength = 20)
    val helpGenerator = composeColumnGenerator(
      wrapColumnGenerator(attributeColumnGenerator(AttrHelpText), 60),
      prefixTextColumnGenerator(attributeColumnGenerator(AttrFallbackValue), "Default value: "))

    val optionsFilter = createOptionsFilter(context.parameterContext)
    val tableParams = generateHelpTable(modelContext, filterFunc = InputParamsFilterFunc,
      sortFunc = inputParamSortFunc(modelContext))(paramNameGenerator, helpGenerator)
    val tableOptions = generateHelpTable(modelContext,
      filterFunc = optionsFilter)(keyGenerator, helpGenerator)
    val helpTexts = renderHelpTables(List(tableParams, tableOptions))
    val buf = generateFailureReport(context)
    buf.append("Usage: transfer [options] ")
      .append(generateInputParamsOverview(modelContext).mkString(" "))
      .append(CR)
      .append(CR)
      .append(helpTexts.head)
      .append(CR)
      .append(CR)
      .append("The following options and switches are supported,")
      .append(CR)
      .append("(parameters marked with * are mandatory:")
      .append(CR)
      .append(CR)
      .append(helpTexts(1))
    buf.toString()
  }

  /**
   * Returns an extractor for the configuration for encryption. Whether
   * encryption is enabled (and additional properties must be provided),
   * depends on the crypt mode.
   *
   * @return the extractor for the ''CryptConfig''
   */
  private[transfer] def cryptConfigExtractor: CliExtractor[Try[CryptConfig]] = {
    conditionalValue(cryptEnabledExtractor, ifExt = definedCryptConfigExtractor,
      elseExt = constantExtractor(Success(DisabledCryptConfig)), ifGroup = Some(GroupEncryption))
  }

  /**
   * Returns an extractor for the configuration of an HTTP server. All
   * properties of this configuration are mandatory; the extractor is applied
   * only if the target server is actually an HTTP server.
   *
   * @return the extractor for the ''HttpServerConfig''
   */
  private[transfer] def httpServerConfigExtractor: CliExtractor[Try[ServerConfig]] = {
    val extUsr = optionValue("user", Some(HelpHttpServerUser))
      .mandatory
    val extPwd = passwordExtractor("password", "HTTP server password", HelpHttpServerPassword)
    for {
      user <- extUsr
      pwd <- extPwd
    } yield createRepresentation(user, pwd)(HttpServerConfig)
  }

  /**
   * Returns an extractor for the ''TransferCommandConfig''. This is the
   * top-level extractor which combines all other extractors to process the
   * full command line.
   *
   * @return the extractor for the ''TransferCommandConfig''
   */
  private def createTransferCommandConfigExtractor(): CliExtractor[Try[TransferCommandConfig]] = {
    for {
      cmd <- commandConfigExtractor
      svr <- serverConfigExtractor
      crypt <- cryptConfigExtractor
      trans <- transferConfigExtractor
    } yield createRepresentation(cmd, svr, crypt, trans)(TransferCommandConfig)
  }

  /**
   * Returns an extractor for the ''TransferConfig''.
   *
   * @return the extractor for the ''TransferConfig''
   */
  private def transferConfigExtractor: CliExtractor[Try[TransferConfig]] = {
    val extSrcFiles = inputValues(fromIdx = 1, toIdx = -2, optKey = Some("transferFiles"),
      optHelp = Some(HelpTransferFiles))
      .multiplicity(atLeast = 1)
      .toPath
      .map(_.map(_.toList))
    val extServerUri = serverUriExtractor.mandatory
    val extChunkSize = optionValue("chunk-size", Some(HelpChunkSize))
      .toInt
      .fallbackValue(DefaultChunkSize)
      .mandatory
      .alias("s")
    val extTimeout = optionValue("timeout", Some(HelpTimeout), allowOverride = true)
      .alias("t")
      .toInt
      .mapTo(t => t.seconds)
    val extLogs = optionValues("log", Some(HelpLog))
      .alias("l")
    val extTag = optionValue("tag", Some(HelpTag))
      .alias("T")
    val extDryRun = switchValue("dry-run", Some(HelpDryRun))
      .alias("d")
    val extAttributes = optionValues("attribute", Some(HelpAttribute))
      .alias("A")
      .mapTo(toAttribute)

    // log level
    val extLogDebug = switchValue("debug", optHelp = Some(HelpLogDebug))
    val extLogInfo = switchValue("info", optHelp = Some(HelpLogInfo))
    val extLogWarn = switchValue("warn", optHelp = Some(HelpLogWarn))
    val extLogError = switchValue("error", optHelp = Some(HelpLogError))
    val extLogLevel = excludingSwitches(true, extLogDebug, extLogInfo, extLogWarn, extLogError)
      .fallbackValue("warn")
      .mandatory

    for {
      srcFiles <- extSrcFiles
      serverUri <- extServerUri
      logs <- extLogs
      tag <- extTag
      chunkSize <- extChunkSize
      timeout <- extTimeout
      dryRun <- extDryRun
      attributes <- extAttributes
      logLevel <- extLogLevel
    } yield createRepresentation(srcFiles, serverUri, chunkSize, timeout, dryRun, logs, tag,
      attributes, logLevel)(TransferConfig)
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
    val groupExtractors = Map(CommandUpload -> uploadCommandConfigExtractor,
      CommandDownload -> downloadCommandConfigExtractor)
    conditionalGroupValue(commandExtractor, groupExtractors)
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
   * An extractor to determine the server type based on its URI. This type
   * determines, which additional options are supported to configure the
   * server.
   */
  private lazy val serverTypeExtractor: CliExtractor[Try[String]] =
    serverUriExtractor.mapTo { uri =>
      if (uri.startsWith("http://") || uri.startsWith("https://")) ServerTypeHttp else ServerTypeFile
    }.mandatory

  /**
   * The extractor for the URI of the target server. The URI is read from the
   * last input parameter.
   */
  private lazy val serverUriExtractor: CliExtractor[SingleOptionValue[String]] =
    inputValue(optKey = Some("serverUri"), index = -1, optHelp = Some(HelpTransferServer))

  /** The extractor for the command passed to the application. */
  private lazy val commandExtractor: CliExtractor[Try[String]] =
    inputValue(index = 0, optKey = Some("transferCommand"), optHelp = Some(HelpTransferCommand))
      .toLower
      .mandatory

  /**
   * Returns an extractor for a (mandatory) password option. The extractor
   * reads the password from the console if it has not been specified on the
   * command line.
   *
   * @param key    the key of the option
   * @param prompt the string to prompt the user
   * @param help   the help text for the password option
   * @return the password extractor for this key
   */
  private def passwordExtractor(key: String, prompt: String, help: String): CliExtractor[Try[String]] =
    optionValue(key, Some(help))
      .fallback(consoleReaderValue(key, optPrompt = Some(prompt)))
      .mandatory

  /**
   * An extractor that extracts a crypt mode value from a command line option.
   */
  private lazy val cryptModeExtractor: CliExtractor[Try[CryptMode.Value]] =
    optionValue("crypt-mode", Some(HelpCryptMode))
      .alias("c")
      .toUpper
      .toEnum(CryptMode.Literals.get)
      .fallbackValue(CryptMode.None)
      .mandatory

  /**
   * An extractor that determines whether encryption is enabled or disabled.
   */
  private lazy val cryptEnabledExtractor = cryptModeExtractor
    .map(triedMode => triedMode.map(_ != CryptMode.None))

  /**
   * Returns an extractor for the ''CryptConfig'' if the crypt mode is set to
   * something different than ''None''. Only then additional options are
   * evaluated.
   *
   * @return the extractor for the defined ''CryptConfig''
   */
  private def definedCryptConfigExtractor: CliExtractor[Try[CryptConfig]] = {
    val extCryptPass = passwordExtractor("crypt-password", "Encryption password", HelpCryptPassword)
    val extCryptAlg = optionValue("crypt-alg", Some(HelpCryptAlg))
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
    val extUploadHashes = switchValue("upload-hashes", Some(HelpUploadHashes))
      .alias("H")
    val extRemoveUploaded = switchValue("remove-uploaded", Some(HelpRemoveUploaded))
      .alias("C")
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
    val extTargetPath = optionValue("target-folder", Some(HelpTargetFolder))
      .toPath
      .mandatory
    val extOverride = switchValue("skip-existing", presentValue = false, optHelp = Some(HelpSkipExisting))
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
    val extRootPath = optionValue("root-path", Some(HelpFileServerRoot))
    val extUmask = optionValue("umask", Some(HelpFileServerUmask))
      .toInt
      .fallbackValueWithDesc(Some("read-only"), DefaultUmask)
      .mandatory
    for {
      rootPath <- extRootPath
      mask <- extUmask
    } yield createRepresentation(rootPath, mask)(FileServerConfig)
  }

  /**
   * Returns the filter function for the help table for options. This filter
   * selects only options and switches that are relevant for the parameters
   * entered by the user.
   *
   * @param extrCtx the current ''ExtractionContext''
   * @return the filter for the help table for options
   */
  private def createOptionsFilter(extrCtx: ExtractionContext): ParameterFilter = {
    import HelpGenerator._
    val cryptGroupExtractor = conditionalGroupExtractor(cryptEnabledExtractor, GroupEncryption)
    val contextFilter = contextGroupFilterForExtractors(extrCtx,
      List(commandExtractor, serverTypeExtractor, cryptGroupExtractor))
    andFilter(negate(InputParamsFilterFunc), contextFilter)
  }

  /**
   * Checks whether during parameter processing errors were encountered. If so,
   * a corresponding report is generated. The resulting ''StringBuilder'' can
   * be appended with other help information.
   *
   * @param context the ''ProcessingContext''
   * @return a ''StringBuilder'' with error information if available
   */
  private def generateFailureReport(context: ProcessingContext): StringBuilder =
    context.optFailureContext map { failureContext =>
      import HelpGenerator._
      val colKey = parameterAliasColumnGenerator()
      val colErr = wrapColumnGenerator(attributeColumnGenerator(AttrErrMessage), 60)
      val buf = new StringBuilder(8192)
      buf.append(ErrorHeader)
        .append(CR)
        .append(CR)
        .append(generateParametersHelp(failureContext)(colKey, colErr))
        .append(CR)
        .append(CR)
      buf
    } getOrElse new StringBuilder(4096)

  /**
   * Returns the exception generator function used by this application.
   *
   * @return the custom exception handler function
   */
  private def exceptionGenerator: ExceptionGenerator = {
    val Messages = Map(FailureCodes.UnsupportedParameter -> "This parameter is not supported.",
      FailureCodes.MultipleValues -> ("For this parameter only a single value is permitted. " +
        "You entered: \"{0}\""))
    val defExGen = ParameterManager.exceptionGenerator(Messages)
    (key, code, params) =>
      key match {
        case ParameterKey("transferCommand", _, _) if code == FailureCodes.UnknownGroup =>
          new IllegalArgumentException(s"Unknown transfer command: '${params.head}. Valid commands are <${params(1)}>")
        case _ =>
          defExGen(key, code, params)
      }
  }

  /**
   * Returns the exception mapper function used by this application. This
   * function aims to produce better error messages for some standard
   * exceptions.
   *
   * @return the exception mapper function
   */
  private def exceptionMapper: ExceptionMapper = (key, optElem) => {
    case e: NumberFormatException =>
      new IllegalArgumentException("Input cannot be converted to a number: \"" + optElem.get.value + "\"", e)
    case e: IllegalArgumentException if key.key == "crypt-mode" =>
      new IllegalArgumentException("This is not a valid encryption mode: \"" + optElem.get.value + "\"", e)
  }

  /**
   * Transforms a parameter value to an ''Attribute''. The parameter value
   * must contain a key and a value separated by ''=''. If this is not the
   * case, this mapping function throws an exception.
   *
   * @param input the original parameter value
   * @return the resulting ''Attribute''
   */
  private def toAttribute(input: String): Attribute =
    input match {
      case RegAttribute(key, value) => Attribute(key, value)
      case _ => throw new IllegalArgumentException("Invalid attribute value: \"" + input +
        "\" - attributes must have the form <key>=<value>.")
    }
}
