#lang scribble/manual
@(require (for-label racket))

@title{Using Extensions}
 Using Extensions@section{categories}
  Internals
@section{related}
  Guides/WritingUGens, Guides/UsingQuarks, Guides/WritingClasses

SC supports extensions to its class library, documentation, and server UGen plugins. Extensions should be packaged as a
single folder containing all three (for convenient addition or removal), or any combination, which can then be placed in
platform-specific extension directories in order to be included.



@section{section}
  Platform Specific Directories

You can install extensions simply by copying the extensions to the following location. There are different directories
for per-user and system-wide extensions that apply to all users.. The locations can be obtained by running

@racketblock[Platform.userExtensionDir:: and ]

@racketblock[Platform.systemExtensionDir::.

Typical user-specific extensions directories:
]
@section{table}
 
## macOS     || ~/Library/Application Support/SuperCollider/Extensions/
## Linux   || ~/.local/share/SuperCollider/Extensions/
::

Typical system-wide extension directories:
@section{table}
 
## macOS     || /Library/Application Support/SuperCollider/Extensions/
## Linux   || /usr/share/SuperCollider/Extensions/
::

@section{section}
  How Extensions Folders Should be Organised

Class files and UGen plugins are recognised by their file extensions. Anything placed within a folder named 
teletype::ignore/:: (case insensitive) will be ignored when compiling the class library or loading plugins.

Here is an example folder layout:

teletype::MyExtension::
@section{tree}
 
## teletype::classes::
    @section{tree}
 
    ## teletype::myClass.sc::
    ## teletype::myUGens.sc::
    ::
## teletype::plugins::
    @section{tree}
 
    ## teletype::myUGenPlugins.scx::
    ::
## teletype::HelpSource::
    @section{tree}
 
    ## teletype::Classes::
        @section{tree}
 
        ## teletype::MyClass.schelp::
        ## teletype::MyUGen1.schelp::
        ## teletype::MyUGen2.schelp::
        ::
    ## teletype::Guides::
        @section{tree}
 
        ## teletype::MyExtensionGuide.schelp::
        ::
    ::
::



