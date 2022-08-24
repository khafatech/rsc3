#lang scribble/manual
@(require (for-label racket))

@title{UnaryOpUGen}
 Apply a unary operation to the values of an input ugen@section{categories}
  UGens>Algebraic
@section{related}
  Classes/BinaryOpUGen, Classes/UnaryOpFunction, Classes/Punop, Overviews/Operators

@section{description}

UnaryOpUGens are created as the result of a unary operator applied to a link::Classes/UGen::.

@racketblock[
(SinOsc.ar(200).abs).dump;
(LFSaw.ar(200).sin).dump;
::
As in the examples given here, you don't usually need to instantiate UnaryOpUGen yourself.

The unary and binary operators are defined in link::Classes/UGen::'s superclass link::Classes/AbstractFunction::, which creates the
BinaryOpUGen as a result of the operation.

See link::Overviews/Operators:: for an overview of common operators.

]
@section{classmethods}
 

@section{method}
 new
return a new instance that applies the operator 
@racketblock[selector:: to the ugen ]

@racketblock[a::

]
@section{argument}
  selector
The selector symbol for the unary operator
@section{argument}
  a
operand

@section{examples}
 

@racketblock[
a = WhiteNoise.ar; // a WhiteNoise
b = a.squared; // a UnaryOpUGen.
b.operator; // squared

// sound example

{ var a = LFSaw.ar(300).range(0, 2pi); a.sin * 0.1 }.play;

// Plotting the "abs" unary operator (via the server):

{ SinOsc.ar(300).abs }.plot
::
]


