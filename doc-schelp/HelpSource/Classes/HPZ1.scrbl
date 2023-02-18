#lang scribble/manual
@(require (for-label racket))

@title{HPZ1}
 Two point difference filter@section{related}
  Classes/LPZ1, Classes/HPZ2
@section{categories}
   UGens>Filters>Linear


@section{description}


A special case fixed filter. Implements the formula:


@racketblock[
out(i) = 0.5 * (in(i) - in(i-1))
::

Which is a two point differentiator.


]
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
// Compare:

{ WhiteNoise.ar(0.25) }.play;

{ HPZ1.ar(WhiteNoise.ar(0.25)) }.play;

// HPZ1 is useful to detect changes in a signal:
// see also HPZ2
(
{
	var changingSignal = LFNoise0.ar(1000);
	var hpz1 = HPZ1.ar(changingSignal);
	[hpz1, hpz1 > 0, hpz1.abs > 0]
}.plot
);
::

]


