#lang scribble/manual
@(require (for-label racket))

@title{LanguageConfig}
 Access and modify interpreter configuration@section{categories}
  Language

@section{description}

The LanguageConfig class provides access to the interpreter configuration.


@section{section}
  Configuration File Format
The configuration file is stored in YAML format, which contains one dictionary. The semantics of the dictionary is listed in
the following table:

@section{definitionList}
 
## teletype::includePaths:: || List of class library paths.
## teletype::excludePaths:: || List of paths to exclude from the class library files (overrides teletype::includePaths::).
## teletype::postInlineWarnings:: || Boolean flag to post warnings about missing inline opportunities.
::

@section{CLASSMETHODS}
 

@section{METHOD}
  store
Store the current configuration to file.

@section{argument}
  file
Path to the configuration file to store. If the value is 
@racketblock[nil:: it defaults to ]

@racketblock[Platform.userConfigDir +/+ "sclang_conf.yaml"::

]
@section{subsection}
  Library Path Handling

The language configuration mechanism provides a way to add or exclude specific paths for the class library.

@section{note}
 Changes to the class library paths won't have any effect before the configuration file is stored and the class library is recompiled.::

@section{METHOD}
  includePaths
Return the class library include paths.

@section{METHOD}
  addIncludePath
Add new class library include path.

@section{METHOD}
  removeIncludePath
Remove path from class library include paths.

@section{METHOD}
  excludePaths
Return the class library exclude paths.

@section{METHOD}
  addExcludePath
Add new class library exclude path.

@section{METHOD}
  removeExcludePath
Remove path from class library exclude paths.

@section{METHOD}
  currentPath
Return the current config file path.

@section{subsection}
  Compiler Warnings

@section{METHOD}
  postInlineWarnings
Get or set the compiler flag, whether warnings should be posted if a FunctionDef cannot be inlined.


@racketblock[
LanguageConfig.postInlineWarnings_(true) // warn
if(0.5.coin) { var x; x = 10.rand; x + 1 } { 10 };
LanguageConfig.postInlineWarnings_(false) // ignore it.
if(0.5.coin) { var x; x = 10.rand; x + 1 } { 10 };
::
]


