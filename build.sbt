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

/** Definition of versions. */
lazy val VersionScala = "2.13.2"
lazy val VersionScalaTest = "3.1.1"

scalacOptions ++= Seq("-deprecation", "-feature")

lazy val SCli = (project in file("."))
  .settings(
    version := "0.1-SNAPSHOT",
    scalaVersion := VersionScala,
    libraryDependencies += "org.scalatest" %% "scalatest" % VersionScalaTest % Test,
    organization := "com.github.oheger",
    name := "scli"
  )
