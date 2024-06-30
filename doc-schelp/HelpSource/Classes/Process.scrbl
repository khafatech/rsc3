#lang scribble/manual
@(require (for-label racket))

@title{Process}
@section{categories}
 Core>Kernel
 Runtime environment for the virtual machine and interpreter.
@section{description}

A Process is the runtime environment for the virtual machine and interpreter.
It has a subclass named link::Classes/Main:: which is where you should override the methods
of Process. There are two methods of interest. One is named 
@racketblock[startup:: and is
called after the class library has been compiled. The other is named ]

@racketblock[run:: and
is called when the user chooses the Run menu command.

]
@section{classMethods}
 

@section{method}
 tailCallOptimize
Get or set tail call optimization. The default is on. Setting this to 
@racketblock[false:: can help with debugging by including intermediate levels in an error backtrace.

]
@section{instanceMethods}
 

@section{method}
 nowExecutingPath

Usage: 
@racketblock[thisProcess.nowExecutingPath::

Returns the full path to the file containing the code that is currently executing emphasis::interactively:: in the interpreter. Usually this is the current document. If the code block executes another file on disk, using link::Classes/String#-load:: or link::Classes/String#-loadPaths::, teletype::nowExecutingPath:: will be the location of the the executed file.

teletype::nowExecutingPath:: is valid only for interactive code, i.e., code files with a teletype::.scd:: extension. It does not apply to class definitions (teletype::.sc::). For that, use ]

@racketblock[thisMethod.filenameSymbol:: or ]

@racketblock[this.class.filenameSymbol::.

This method is supported in the SuperCollider IDE, the macOS-only SuperCollider.app, and the scel (SuperCollider-Emacs-Lisp) environment. In other editor environments, it will return ]

@racketblock[nil::.

]
@section{WARNING}
  teletype::nowExecutingPath:: has a corresponding setter method, teletype::nowExecutingPath_::, for internal use only by the interpreter. Do not call the setter method!::

@section{method}
 startup

called after the class library has been compiled. Override this in class link::Classes/Main:: to do whatever you want.

@section{method}
 run

called when the user chooses the Run menu command. Override this in class link::Classes/Main:: to do whatever you want.

@section{method}
 mainThread

The top-level link::Classes/Thread::, i.e the link::Classes/Thread#-parent#parent:: of all
other Threads. This instance of Thread always exists and is created with the Process when
SuperCollider starts.

@section{discussion}
 

All SuperCollider code initially runs in the context of the main Thread:

@section{list}
 
## Code evaluated in code editor
## Code evaluated on command line
## Tasks scheduled on any link::Classes/Clock::
## Functions evaluated in response to incoming OSC and MIDI messages
::

This means that link::Classes/Thread#.thisThread#thisThread:: will always initially point
to the main Thread. However, when some code starts a link::Classes/Routine::, the Routine
becomes the current Thread, with the main Thread as its parent.

