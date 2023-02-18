#lang scribble/manual
@(require (for-label racket))

@title{Pprotect}
 evaluate a function when an error occured in the thread@section{related}
  Classes/Ptrace
@section{categories}
  Streams-Patterns-Events>Patterns>Language Control

@section{ClassMethods}
 

@section{method}
 new

@section{argument}
 pattern
any pattern

@section{argument}
 func
a link::Classes/Function:: to be evaluated when an error occurs. The error and the thread are passed as arguments to the function.

@section{Examples}
 


@racketblock[
(
var x;
var func = { "an error happened".postln };
a = Pprotect(Pseq([1, 3, 3, Pfuncn({ Error.throw }), 2]), func);
x = Pbind(\degree, a, \dur, 0.5).play;
)
::
]


