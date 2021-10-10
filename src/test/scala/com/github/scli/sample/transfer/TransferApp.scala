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

import com.github.scli.sample.transfer.TransferParameterManager.{DownloadCommandConfig, TransferCommandConfig, UploadCommandConfig}

/**
 * An object simulating the transfer sample application.
 *
 * This object demonstrates how a command line can be parsed and the result can
 * be evaluated. If command line processing was successful, the parameters of
 * the transfer operation are just printed to the console. If the user has
 * provided invalid parameters or has requested help, help information is
 * printed. If invalid parameters have been discovered, corresponding error
 * messages are printed as well.
 */
object TransferApp {
  def main(args: Array[String]): Unit = {
    TransferParameterManager.evaluateCommandLine(args.toIndexedSeq) match {
      case Right(config) => transfer(config)
      case Left(context) =>
        println(TransferParameterManager.generateHelp(context))
    }
  }

  private def transfer(config: TransferCommandConfig): Unit = {
    println("Executing transfer operation.")
    val action = config.commandConfig match {
      case _: UploadCommandConfig => "Uploaded"
      case _: DownloadCommandConfig => "Downloaded"
    }
    print(s"$action ${config.transferConfig.sourceFiles.size} files")
    println(s" using target server ${config.transferConfig.serverUrl}")
    println("Full config is")
    println(config)
  }
}
