#lang scribble/manual
@(require (for-label racket))

@title{Quarks}

@title{Quarks}
 Package manager@section{categories}
  Quarks
@section{related}
  Guides/UsingQuarks, Classes/Quark

@section{description}

See link::Guides/UsingQuarks:: for an introduction to the Quarks package system.

@section{CLASSMETHODS}
 

@section{METHOD}
  gui
Show the interface for managing quarks
@section{returns}
  QuarksGui

@section{METHOD}
  install
@section{ARGUMENT}
  name
Name of a quark that is listed in the directory,
or the url of a git repository
or the path (absolute or relative to current working directory) of a folder to install.
@section{ARGUMENT}
  refspec
Optional git refspec. By default it will install the latest version.
Optionally you can specify a tag: "tags/1.0.0"
A sha commit: "15e6ea822a18d06b286c3f10918f83b8d797d939"
"HEAD"
nil (default)
@section{returns}
  this

@section{METHOD}
  installQuark
Install a quark
Usually you use *install with a name, url or path.
@section{ARGUMENT}
  quark
@section{returns}
  this

@section{METHOD}
  uninstall
@section{ARGUMENT}
  name
Name (String) of a quark that is listed in the directory,
or url of a git repository
or the path (absolute or relative to current working directory) of a folder to uninstall.
@section{returns}
  this

@section{METHOD}
  clear
Uninstall all Quarks, by setting LanguageConfig.installedPaths to empty.
@section{returns}
  this

@section{METHOD}
  addFolder
@section{ARGUMENT}
  path
In addition to the default downloaded-quarks
add folders that contain quarks to offer on the menu for installation.
These may be private quarks, cloned working copies or folders where you have manually downloaded quarks.
@section{NOTE}
  The argument should be a path to a directory emphasis::containing quark directories::. It should emphasis::not:: be an isolated quark directory by itself. Users are discouraged from scattering quark directories in isolated locations. ::
@section{returns}
  this

@section{METHOD}
  all
All Quarks whether downloaded or installed or not. Includes any Quarks that were installed by path.
@section{returns}
  Array of Quarks

@section{METHOD}
  installed
All currently installed Quarks
@section{returns}
  Array of Quarks

@section{METHOD}
  isInstalled
@section{ARGUMENT}
  name
Name, url or path
@section{returns}
  Boolean

@section{METHOD}
  save
Saves the currently installed quarks to a file as a list of urls and refspecs.
@section{ARGUMENT}
  path
path of file to save to
@section{returns}
  this

@section{METHOD}
  load
Clear all installed quarks and load a list from a file.
Relative paths in the file are resolved relative to the file itself.
eg. ./classes/my-quark
Unix style tildas (~/supercollider/quarks/my-quark) resolve to the user's home directory, even on Windows.
By convention the file is called quarks.txt
@section{ARGUMENT}
  path
path of file to load. May contain ~ or relative paths (root is current working directory)
@section{ARGUMENT}
  done
function to be evaluated when loading is done

@section{returns}
  this

@section{METHOD}
  update
Runs 'git pull' on the checked out copy of the quark. The gui provides a more robust way to do updates.
@section{ARGUMENT}
  name
name of quark
@section{returns}
  this

@section{METHOD}
  openFolder
Open the downloaded-quarks folder
@section{returns}
  this

@section{METHOD}
  folder
Path of the downloaded-quarks folder where Quarks are cloned to before installing.
@section{returns}
  path

@section{METHOD}
  fetchDirectory
Private.
Fetches the directory listing into downloaded-quarks/quarks
If a local copy already exists and it is not a git repo then this is used instead.
@section{ARGUMENT}
  force
(Boolean)
Force fetch. By default it is fetched once per session. Recompile the class library to fetch it again, or
call Quarks.fetchDirectory(true) to force it.
@section{returns}
  this

@section{METHOD}
  classesInPackage
Returns the Classes that are defined in the Quark or package.
@section{ARGUMENT}
  packageName
name of quark or any folder in Extensions or Common.
"Common" is a package that refers to the standard library.
@section{returns}
  Array of Classes

@section{METHOD}
  link
Adds the path to LanguageConfig.installedPaths.
private
@section{ARGUMENT}
  path
@section{returns}
  this

@section{METHOD}
  unlink
Removes a path from LanguageConfig.installedPaths.
private
@section{ARGUMENT}
  path
@section{returns}
  this

@section{METHOD}
  initClass
private
@section{returns}
  this

@section{METHOD}
  findQuarkURL
private
@section{ARGUMENT}
  name
@section{returns}
  this

@section{METHOD}
  directoryUrl
The URL of the directory.txt file
@section{returns}
  this

@section{METHOD}
  directory
The community contributed Quarks directory. Fetched from the directoryUrl
and parsed.
@section{returns}
  Dictionary[name->url@refspec]

@section{METHOD}
  asAbsolutePath
Helper method to resolve paths to absolute paths.
@section{ARGUMENT}
  path
@section{ARGUMENT}
  relativeTo
optional root for resolving relative paths
@section{returns}
  absolute path

@section{METHOD}
  quarkNameAsLocalPath
private
@section{ARGUMENT}
  name
quark name, path or git url.
@section{returns}
  absolute path where the Quark is

@section{METHOD}
  at
private. gets or creates a Quark by name, storing it in a central cache.
@section{ARGUMENT}
  name
@section{returns}
  Quark

@section{PRIVATE}
  prReadDirectoryFile



@section{INSTANCEMETHODS}
 


