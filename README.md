Docs
===========

[![Build Status](https://www.travis-ci.org/edadma/docs.svg?branch=master)](https://www.travis-ci.org/edadma/docs)
[![Build status](https://ci.appveyor.com/api/projects/status/iophnk3cycjtf8px?svg=true)](https://ci.appveyor.com/project/edadma/docs)
[![Coverage Status](https://coveralls.io/repos/github/edadma/docs/badge.svg?branch=master)](https://coveralls.io/github/edadma/docs?branch=master)
[![License](https://img.shields.io/badge/license-ISC-blue.svg)](https://github.com/edadma/docs/blob/master/LICENSE)
[![Version](https://img.shields.io/badge/latest_release-v0.1-orange.svg)](https://github.com/edadma/docs/releases/tag/v0.1)

*Docs* is a simple documentation written in [Scala](http://scala-lang.org). Docs is inspired by [MdBook](https://rust-lang-nursery.github.io/mdBook/) and [MkDocs](https://www.mkdocs.org/), and it is not difficult to port documentation to Docs.

Examples
--------



Usage
-----

### Library

Use the following definition to use Docs in your Maven project:

```xml
<repository>
  <id>hyperreal</id>
  <url>https://dl.bintray.com/edadma/maven</url>
</repository>

<dependency>
  <groupId>xyz.hyperreal</groupId>
  <artifactId>docs</artifactId>
  <version>0.1</version>
</dependency>
```

Add the following to your `build.sbt` file to use Docs in your SBT project:

```sbt
resolvers += "Hyperreal Repository" at "https://dl.bintray.com/edadma/maven"

libraryDependencies += "xyz.hyperreal" %% "docs" % "0.1"
```

### Executable

An executable can be downloaded from [here](https://dl.bintray.com/edadma/generic/docs-0.1.jar). *You do not need* the Scala library for it to work because the JAR already contains all dependencies. You just need Java 8+ installed.

Run it as a normal Java executable JAR with the command `java -jar docs-0.1.jar init <path>` in the folder where you downloaded the file, where *path* is the path to the documentation project folder.

Building
--------

### Requirements

- Java 8
- SBT 1.1.6+
- Scala 2.12.6+

### Clone and Assemble Executable

```bash
git clone git://github.com/edadma/docs.git
cd docs
sbt assembly
```

The command `sbt assembly` also runs all the unit tests.


License
-------

ISC © 2018 Edward A. Maxedon, Sr.