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
lazy val VersionScala213 = "2.13.6"
lazy val VersionScala212 = "2.12.15"
lazy val VersionScala211 = "2.11.12"
lazy val supportedScalaVersions = List(VersionScala213, VersionScala212, VersionScala211)
lazy val VersionScalaTest = "3.1.1"
lazy val VersionMockito = "1.9.5"
lazy val VersionScalaTestMockito = "1.0.0-M2"
lazy val VersionJunit = "4.13" // needed by mockito

scalacOptions ++= Seq("-deprecation", "-feature")

lazy val testDependencies = Seq(
  "org.scalatest" %% "scalatest" % VersionScalaTest % Test,
  "org.scalatestplus" %% "scalatestplus-mockito" % VersionScalaTestMockito % Test,
  "org.mockito" % "mockito-core" % VersionMockito % Test,
  "junit" % "junit" % VersionJunit % Test
)

lazy val SCli = (project in file("."))
  .settings(
    version := "1.1.0-SNAPSHOT",
    scalaVersion := VersionScala213,
    crossScalaVersions := supportedScalaVersions,
    libraryDependencies ++= testDependencies,
    organization := "com.github.oheger",
    homepage := Some(url("https://github.com/oheger/scli")),
    name := "scli",
    description := "A small functional library for Scala to process command line parameters",
    licenses += ("Apache-2.0", url("https://www.apache.org/licenses/LICENSE-2.0.html")),
    scmInfo := Some(
      ScmInfo(
        url("https://github.com/oheger/scli.git"),
        "scm:git:git@github.com:oheger/scli.git"
      )
    ),
    developers := List(
      Developer(
        id = "oheger",
        name = "Oliver Heger",
        email = "oheger@apache.org",
        url = url("https://github.com/oheger")
      )
    ),
    publishMavenStyle := true,
    pomIncludeRepository := { _ => false },
    publishTo := {
      val nexus = "https://oss.sonatype.org/"
      if (isSnapshot.value)
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases" at nexus + "service/local/staging/deploy/maven2")
    }
  )

Compile / packageBin / mappings += {
  (baseDirectory.value / "LICENSE.txt") -> "META-INF/LICENSE.txt"
}

Compile / packageSrc / mappings ++= Seq({
  (baseDirectory.value / "LICENSE.txt") -> "META-INF/LICENSE.txt"
},
  {
    (baseDirectory.value / "README.adoc") -> "README.adoc"
  },
  {
    (baseDirectory.value / "Tutorial.adoc") -> "Tutorial.adoc"
  })

Compile / packageDoc / mappings += {
  (baseDirectory.value / "LICENSE.txt") -> "META-INF/LICENSE.txt"
}
