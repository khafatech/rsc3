#lang scribble/manual
@(require (for-label racket))

@title{Pipe}
 pipe stdin to, or stdout from, a UNIX shell command@section{related}
  Classes/UnixFILE
@section{categories}
  Files

@section{description}

Pipe stdin to, or stdout from, a UNIX shell command. Pipe treats the shell command as if it were a UnixFILE, and returns nil when done. See link::Classes/UnixFILE:: for details of the access methods. Pipe must be explicitly closed. Do not rely on the garbage collector to do this for you!

@section{ClassMethods}
 

@section{method}
 new

@section{argument}
 commandLine
A link::Classes/String:: representing a valid shell command.

@section{argument}
 mode
A link::Classes/String:: representing the mode. Valid modes are "w" (pipe to stdin) and "r" (pipe from stdout).

@section{InstanceMethods}
 

@section{private}
 prClose, prOpen

@section{method}
 open
Open the file.

@section{argument}
 commandLine
A command line link::Classes/String:: passed to popen.

@section{argument}
 mode
A link::Classes/String:: passed to popen, so should be one of: "r","w"

@section{method}
 close
Closes the pipe, waiting for the command to finish. You must do this explicitly before the Pipe object is garbage collected.

@section{returns}
  The exit status of the command (an Integer).

@section{Examples}
 

@section{note}
 
For anyone still using macOS 10.3, UNIX commands like pipe do not work when the server is booted; quit the server, otherwise SuperCollider crashes. More recent macOS is not affected.

::


@racketblock[
// this pipes in stdout from ls
(
var p, l;
p = Pipe.new("ls -l", "r");			// list directory contents in long format
l = p.getLine;					// get the first line
while({l.notNil}, {l.postln; l = p.getLine; });	// post until l = nil
p.close;					// close the pipe to avoid that nasty buildup
)
::

A more time-intensive request:
]

@racketblock[
(
var p, l;
p = Pipe.new("ping -c10 sourceforge.net", "r");	// list directory contents in long format
l = p.getLine;					// get the first line
while({l.notNil}, {l.postln; l = p.getLine; });	// post until l = nil
p.close;					// close the pipe to avoid that nasty buildup
)
::
]


