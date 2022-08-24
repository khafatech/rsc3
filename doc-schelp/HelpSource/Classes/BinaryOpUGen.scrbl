#lang scribble/manual
@(require (for-label racket))

@title{BinaryOpUGen}
 Apply a binary operation to the values of an input UGen@section{related}
  Classes/UnaryOpUGen, Classes/BinaryOpFunction, Classes/Pbinop, Overviews/Operators
@section{categories}
  UGens>Algebraic

@section{description}

BinaryOpUGens are created as the result of a binary operator applied to a link::Classes/UGen::.

@racketblock[
(SinOsc.ar(200) * ClipNoise.ar).dump;
(SinOsc.ar(200).thresh(0.5)).dump;
::
The use of the binary operators ]

@racketblock[*:: and ]

@racketblock[thresh:: above each instantiate a BinaryOpUGen. The operators themselves  (which are methods) are not to be confused with the resulting BinaryOpUGen (which is an object). The unary and binary operators are defined in link::Classes/UGen::'s superclass link::Classes/AbstractFunction::, which creates the
BinaryOpUGen as a result of the operation.

When operating on UGens instead of numbers, what results is not a result of the calculation, but a structure that represents that calculation. For the immediate operations on numbers, see for example link::Classes/SimpleNumber::.

See link::Overviews/Operators:: for an overview of common operators.

]
@section{classmethods}
 
@section{private}
  new1

@section{method}
 new
return a new instance that applies the operator 
@racketblock[selector:: to the UGens ]

@racketblock[a:: and ]

@racketblock[b:: normally, this is implicitly called when applying an operator to a link::Classes/UGen::.
]
@section{argument}
  selector
The selector symbol for the binary operator
@section{argument}
  a
left operand
@section{argument}
  b
right operand
@section{returns}
  A new instance of BinaryOpUGen

@section{instancemethods}
 
@section{private}
  init, optimizeGraph, constantFolding

@section{examples}
 


@racketblock[
a = WhiteNoise.ar; // a WhiteNoise
b = a + 2; // a BinaryOpUGen.
b.operator; // +

// sound example
(
{
	var a = LFSaw.ar(300);
	var b = LFSaw.ar(329.1);
	a % b * 0.1
}.play;
)
::

]
@section{subsection}
 The comparison operators

The operators 
@racketblock[ >, >=, <, <= :: are particularly useful for triggering. They should not be confused with their use in conditionals. Compare:

]

@racketblock[
if(1 > 0) { "1 is greater than 0".postln }; // > returns a boolean
::

with

]

@racketblock[
// trigger an envelope
(
{
    var trig;
    trig = SinOsc.ar(1) > 0.1;
    EnvGen.kr(Env.perc, trig, doneAction: Done.none) * SinOsc.ar(440,0,0.1)
}.play
) // > outputs 0 or 1
::

See link::Overviews/Operators:: or the implementation of these in link::Classes/AbstractFunction:: for more detail.

Since the equality operator ( ]

@racketblock[==:: ) is used to distinguish objects including UGens, it cannot be used to create a BinaryOpUGen by application. Instead, to get a trigger value each time two signals are the same (instead of just finding out whether two UGens are the same), one can instantiate a BinaryOpUGen directly:

]

@racketblock[
(
{
    var a = SinOsc.ar(1).round(0.1);
    var b = SinOsc.ar(1.2).round(0.1);
    BinaryOpUGen('==', a, b) * 0.1
}.play;
)
::

]


