#lang scribble/manual
@(require (for-label racket))

@title{Interpreter}
 The interpreter defines a context in which interactive commands are compiled and executed.@section{categories}
  Core>Kernel
@section{related}
 Guides/How-to-Use-the-Interpreter

@section{description}

The interpreter is an object that handles the translation and execution of code at runtime. It is that what runs any program code and defines a context for it.


@racketblock[
(
a = 5 + 7;
this.cmdLine.postln;
)
::

]
@section{classMethods}
 

@section{private}
 new

@section{instanceMethods}
 

@section{subsection}
 Accessing

In the interpreter, 
@racketblock[this:: refers to the interpreter itself, e.g.: ]

@racketblock[this.postln::

The interpreter defines global variables (]

@racketblock[a:: … ]

@racketblock[z::), that can be used for interactive programming. Except these single letter variables ("interpreter variables"), all variables have to be defined by the keyword ]

@racketblock[var:: (see: link::Reference/Assignment::, and link::Reference/Scope::).

]

@racketblock[
// typical usage
a = 4;
b = 3;
b = b + a;

// some sound
a = Synth(\default);
g = fork { loop { 0.1.wait; a.set(\freq, 200 + 20.0.rand2.postln) } };
g.stop; a.free;

// an overview of all the variables
this.inspect;
::

]
@section{note}
 Use these variables with a bit of extra care – as they are global, they remain in memory and one piece of code may happen to interfere with another one. The variable 
@racketblock[s:: is by convention bound to the default server (link::Classes/Server::) and should not be changed.::



]
@section{method}
 clearAll

set the values of the variables 
@racketblock[a:: through ]

@racketblock[z:: to nil.

]

@racketblock[
x = 123;
x.postln;
this.clearAll;
x.postln;
::

]
@section{subsection}
 Compile & Interpret

@section{method}
 interpret

Compile and execute a link::Classes/String::.


@racketblock[
this.interpret("(123 + 4000).postln");
::

]
@section{method}
 interpretPrint

Compile and execute a link::Classes/String::, printing the result.


@racketblock[
this.interpretPrint("123 + 4000");
::

]
@section{method}
 compile

Compile a String and return a link::Classes/Function::.


@racketblock[
(
z = this.compile("(123 + 4000).postln");
z.postln;
z.value;
)
::

]
@section{method}
 compileFile

Reads the file at pathName, compiles it and returns a Function.
The file must contain a valid SuperCollider expression, naturally.
This will not compile class definitions, only expressions.

@section{method}
 executeFile

Reads the file at pathName, compiles it and executes it, returning the result.
The file must contain a valid SuperCollider expression, naturally.
This will not compile class definitions, only expressions.

@section{method}
 cmdLine

Returns the previously interpreted code.


@racketblock[
1 + 2;
this.cmdLine
::

]
@section{method}
 codeDump

this interpreter variable can be set to evaluate a function with any successfully compiled code.
see e.g. the class History.


@racketblock[
a = [ ]; // store all the code evaluated in a
this.codeDump = { |code| a = a.add(code) };
1 + 3;
f = { "hallo" };
a.postcs;
codeDump = nil; // reset to nil.
::

]
@section{method}
 preProcessor

If this is set to a function, all interactively executed code is piped through it before parsing and
interpreting. This is mostly used for developing domain-specific live coding languages that piggyback
off the SuperCollider editing environment.

This function is called by link::Classes/Interpreter#-interpretPrintCmdLine:: with two arguments:
the code string and the interpreter itself.


@racketblock[
// silly but simple: understand a Saw for every SinOsc
this.preProcessor = { |code| code.replace("SinOsc", "Saw") };

{ SinOsc.ar(200) * 0.1 }.play;

preProcessor = nil; // reset to nil.
::

]
@section{method}
 a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z

Global variables ("interpreter variables") for interactive programming (see link::#Accessing::).

@section{method}
 functionCompileContext

The compiler uses this method as a virtual context in which to compile code.


