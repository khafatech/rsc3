#lang scribble/manual
@(require (for-label racket))

@title{Quark}
 Object for managing a Quark - a package of source code@section{categories}
  Quarks
@section{related}
  Guides/UsingQuarks, Classes/Quarks

@section{description}

A Quark is a folder of source code, a package. It may be cloned from a git repository, or maybe not.
This class is used by the Quarks class and you will not usually want to use it directly.

@section{CLASSMETHODS}
 

@section{METHOD}
  new
@section{ARGUMENT}
  name
Quark name, git url or local path (absolute or relative)
@section{ARGUMENT}
  refspec
@section{ARGUMENT}
  url
@section{ARGUMENT}
  localPath
@section{returns}
  this

@section{METHOD}
  fromLocalPath
alternate constructor
@section{ARGUMENT}
  path
@section{returns}
  this

@section{METHOD}
  fromDirectoryEntry
alternate constructor
@section{ARGUMENT}
  name
@section{ARGUMENT}
  directoryEntry
@section{returns}
  this

@section{METHOD}
  parseQuarkName
@section{ARGUMENT}
  name
@section{ARGUMENT}
  refspec
@section{ARGUMENT}
  url
@section{ARGUMENT}
  localPath
@section{returns}
  this

@section{METHOD}
  parseDependency
private
@section{ARGUMENT}
  dep
@section{ARGUMENT}
  forQuark
@section{returns}
  this

@section{PRIVATE}
 prMakeDep

@section{INSTANCEMETHODS}
 

@section{METHOD}
  name
@section{returns}
  String

@section{METHOD}
  dependencies
Based on the dependencies list in the quark file, returns an array of Quarks.
@section{returns}
  Array of Quark

@section{METHOD}
  deepDependencies
Declared dependencies of this Quark and those of each dependency.
This will check out all dependencies.
@section{returns}
  Array of Quarks

@section{METHOD}
  data
Lazily parses the quark file (if found) and caches it
@section{returns}
  Dictionary - the contents of the quark file

@section{METHOD}
  refspec
Git refspec (tag or sha hash)
@section{returns}
  this

@section{METHOD}
  localPath
Absolute path where the Quark is located
@section{returns}
  this

@section{METHOD}
  summary
Summary text from the quark file
@section{returns}
  this

@section{METHOD}
  url
Git repository url. If not declared when creating, it will examine the checked out git source
and get the origin.
@section{returns}
  this

@section{METHOD}
  isDownloaded
@section{returns}
  Boolean

@section{METHOD}
  isInstalled
@section{returns}
  Boolean

@section{METHOD}
  git
Quarks that have git repos have a Git object that can be used for checking out, listing tags etc.
@section{returns}
  a Git object

@section{METHOD}
  init
private
@section{ARGUMENT}
  argName
@section{ARGUMENT}
  argUrl
@section{ARGUMENT}
  argRefspec
@section{ARGUMENT}
  argLocalPath
@section{returns}
  this

@section{METHOD}
  install
@section{returns}
  this

@section{METHOD}
  uninstall
@section{returns}
  this

@section{METHOD}
  checkout
Clone and checkout the url and refspec.
Used by install and for switching versions.
@section{returns}
  this

@section{METHOD}
  version
@section{returns}
  String

@section{METHOD}
  tags
@section{returns}
  Array of Strings

@section{METHOD}
  isCompatible
Evaluates the 'isCompatible' function in the quarkfile, if there is one.
This allows a quarkfile to check its environment and raise an alarm before it gets installed and breaks something.
@section{returns}
  Boolean

@section{METHOD}
  definesClasses
Classes that are defined by this Quark
@section{returns}
  Array of Classes

@section{METHOD}
  definesExtensionMethods
Methods that this Quark defines that overwrite implementations in other packages including in Common.
@section{returns}
  Array of Methods

@section{METHOD}
  help
Open the help file. Either as specified in the quark file as 'schelp' or searches by the name of the quark.
@section{returns}
  this

@section{METHOD}
  changed
After un/installing or checking out, state is set to changed.
code smell: this is for the gui
@section{returns}
  Boolean

@section{METHOD}
  printOn
@section{ARGUMENT}
  stream
@section{returns}
  this

@section{METHOD}
  parseQuarkFile
private
@section{returns}
  this

@section{PRIVATE}
 prCollectDependencies


