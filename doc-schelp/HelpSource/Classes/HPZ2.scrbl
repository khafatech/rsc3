#lang scribble/manual
@(require (for-label racket))

@title{HPZ2}
 Two zero fixed midcut.@section{related}
  Classes/BPZ2, Classes/BRZ2, Classes/LPZ2, Classes/HPZ1
@section{categories}
   UGens>Filters>Linear


@section{description}


A special case fixed filter. Implements the formula:


@racketblock[
out(i) = 0.25 * (in(i) - (2 * in(i - 1)) + in(i - 2)).
::


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

{ HPZ2.ar(WhiteNoise.ar(0.25)) }.play;

// HPZ2 is useful to detect changes in a signal:
// see also HPZ1
(
{
	var changingSignal = LFNoise0.ar(1000);
	var hpz1 = HPZ2.ar(changingSignal);
	[hpz1, hpz1 > 0]
}.plot
);
::

]


