#lang scribble/manual
@(require (for-label racket))

@title{Platform}
 handle cross-platform differencies@section{categories}
  Platform

@section{description}

The Platform class (along with its subclasses) handles things which differ between operating systems (mac/linux/windows/...), to simplify cross-platform aspects of SuperCollider.

Platform is an abstract class encapsulating various platform dependent constants and properties, such as directories, primitive features and startup files. The platform object is accessible through the 
@racketblock[platform:: method of the main process instance:
]

@racketblock[
thisProcess.platform
::

Currently implemented platforms include: OSXPlatform, LinuxPlatform, WindowsPlatform, UnixPlatform.

]
@section{classmethods}
 
Most of Platforms class methods are simply wrappers to 
@racketblock[thisProcess.platform.]
@section{method}
 .

@section{subsection}
  Platform name and platform dependent actions

@section{method}
  case
Perform actions depending on the current platform (name), just like Object:switch:

@racketblock[
Platform.case(
    \osx,       { "OSX".postln },
    \linux,     { "Linux".postln },
    \windows,   { "Windows".postln }
);
::

]
@section{method}
  ideName
returns a String indicating which IDE the language believes it is running in. (Often this is determined via the "-i" option to the sclang executable.) This is determined when sclang starts and cannot be changed dynamically.

The main purpose of this is to include/exclude folders from the class search patch depending on which IDE is in use: for example, if the value of ideName is "scapp" then folders named "scide_scapp" are included and all other folders beginning with "scide_" are excluded. The default value of this is "none".

Known IDE names in use are "scapp" (SuperCollider.app on Mac), "scvim" (vim), "scel" (emacs). Others may be used.

@section{subsection}
  Directories and filesystem stuff
@section{method}
  classLibraryDir
location of the bundled class library

@section{method}
  helpDir
location of the bundled help files

@section{method}
  systemAppSupportDir
system application support directory

@section{method}
  userAppSupportDir
user application support directory

@section{method}
  userConfigDir
directory for configuration files

@section{method}
  systemExtensionDir
system extension directory (see link::Guides/UsingExtensions::)

@section{method}
  userExtensionDir
user extension directory (see link::Guides/UsingExtensions::)

@section{method}
  platformDir
platform specific directory for class files (see link::Guides/UsingExtensions::)

@section{method}
  pathSeparator
platform specific path separator

@section{method}
  resourceDir
platform specific resource directory

@section{method}
  defaultTempDir
default directory for temporary files

@section{subsection}
  Features

@section{method}
  when
Evaluate ifFunction if all features are present, otherwise evaluate elseFunction.

@racketblock[
Platform.when(#[\Document, \SCWindow], { "yeehah!".postln });
::

]
@section{instancemethods}
 

@section{private}
  shutdown, startup

@section{method}
  name
returns the platform name

@section{method}
  recompile
recompile class library


@section{subsection}
  Directories and filesystem stuff
@section{method}
  classLibraryDir
location of the bundled class library

@section{method}
  helpDir
location of the bundled help files

@section{method}
  systemAppSupportDir
system application support directory

@section{method}
  userAppSupportDir
user application support directory

@section{method}
  userConfigDir
directory for configuration files

@section{method}
  systemExtensionDir
system extension directory (see link::Guides/UsingExtensions::)

@section{method}
  userExtensionDir
user extension directory (see link::Guides/UsingExtensions::)

@section{method}
  platformDir
platform specific directory for class files (see link::Guides/UsingExtensions::)

@section{method}
  pathSeparator
platform specific path separator

@section{method}
  resourceDir
platform specific resource directory

@section{method}
  recordingsDir
recording directory

@section{method}
  defaultTempDir
default directory for temporary files



@section{subsection}
  Startup files

@section{method}
  startupFiles
files to be loaded on startup

@section{method}
  loadStartupFiles
(re)load startup files

@section{subsection}
  Features

Features are abstract symbols that can be declared by extension authors and be checked during runtime in user code. Apart from explicitly declared features, class and primitive names are implicitly declared.

@section{method}
  declareFeature
Declare aSymbol to be a feature present in the runtime. Class names and primitive names cannot be declared as features.

@section{method}
  hasFeature
Return true if the feature aSymbol is present in the runtime system. aSymbol can refer to explicitly declared features as well as class and primitive names.

@racketblock[
thisProcess.platform.hasFeature(\Object);
thisProcess.platform.hasFeature('_SCWindow_BeginFullScreen');
thisProcess.platform.hasFeature('_myFuncyPrimitive');

thisProcess.platform.declareFeature('superCrazyCompositionSystem');
thisProcess.platform.hasFeature('superCrazyCompositionSystem');
::

]
@section{method}
  when
Evaluate ifFunction if all features are present, otherwise evaluate elseFunction.

@racketblock[
thisProcess.platform.when(#[\Document, \SCWindow], { "yeehah!".postln });
::
]


