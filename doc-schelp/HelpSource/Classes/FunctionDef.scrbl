#lang scribble/manual
@(require (for-label racket))

@title{FunctionDef}
 FunctionDefs contain code which can be executed from a Function.@section{categories}
 Core>Kernel
@section{related}
 Classes/Function

@section{description}


@section{subsection}
  Related Keywords

@section{method}
  thisFunctionDef
The global pseudo-variable 
@racketblock[thisFunctionDef:: always evaluates to the
current enclosing FunctionDef.

See also: link::Classes/Function#.thisFunction#thisFunction::

]
@section{instanceMethods}
 

@section{subsection}
 Accessing

Even though it is possible to change the values in the various arrays that define the FunctionDef,
you should not do it, unless you like to crash.

@section{method}
 code

Get the byte code array.


@racketblock[
{ |a = 9, b = 10, c| a + b }.def.code;
::

]
@section{method}
 sourceCode

Get the source code string.

@racketblock[
{ |a = 9, b = 10, c| a + b }.def.sourceCode.postcs;
::

]
@section{method}
 context

Get the enclosing FunctionDef or Method.

@section{method}
 findReferences

return a list of all references to a given symbol.

@section{method}
 argNames

Get the Array of Symbols of the argument names.


@racketblock[
{ |a = 9, b = 10, c| a + b }.def.argNames;
::
]
@section{method}
 prototypeFrame

Get the array of default values for argument and temporary variables.


@racketblock[
{ |a = 9, b = 10, c| a + b }.def.prototypeFrame;
::
]
@section{method}
 varNames

Get the Array of Symbols of the local variable names.


@racketblock[
{ |a = 9, b = 10, c| var x = 9; a + b + x }.def.varNames;
::
]
@section{method}
 argumentString

Return a string that contains  arguments and their default values for embedding in a string


@racketblock[
{ |a = 9, b = 10, c| a + b }.def.argumentString;
::

]
@section{method}
 makeEnvirFromArgs

Get the Array of Symbols of the local variable names.


@racketblock[
{ |a = 9, b = 10, c| a + b }.def.makeEnvirFromArgs;
::

]
@section{subsection}
 Utilities

@section{method}
 dumpByteCodes

"Disassemble" and post the FunctionDef's byte code instructions to the text window.


