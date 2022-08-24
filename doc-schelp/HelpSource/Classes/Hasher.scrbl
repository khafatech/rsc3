#lang scribble/manual
@(require (for-label racket))

@title{Hasher}
 Scrambled value with a hash function.@section{categories}
   UGens>Filters>Nonlinear, UGens>Random

@section{description}


Returns a unique output value from -1 to +1 for each input value
according to a hash function. The same input value will always produce
the same output value. The input need not be in the range -1 to +1.

@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 in
The input signal.

@section{argument}
 mul
Output will be multiplied by this value.

@section{argument}
 add
This value will be added to the output.

@section{Examples}
 


@racketblock[
{ Hasher.ar(Line.ar(0,1,1), 0.2) }.play;

// Even adding a small amount to the input can result in a very different output. Compare these two examples:
(
{
	SinOsc.ar(
		Hasher.kr(MouseX.kr(0,10).round(1), 300, 500)
	) * 0.1
}.play;
)

(
{
	SinOsc.ar(
		Hasher.kr(MouseX.kr(0,10).round(1) + 0.0001, 300, 500)
	) * 0.1
}.play;
)

// Output stays constant while not moving the mouse.
(
{
	SinOsc.ar(
		Hasher.kr(MouseX.kr(0,10), 300, 500)
	) * 0.1
}.play;
)
::
]


