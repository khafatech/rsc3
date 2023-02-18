#lang scribble/manual
@(require (for-label racket))

@title{LeakDC}
 Remove DC@section{related}
  Classes/DC
@section{categories}
   UGens>Filters>Linear


@section{description}


This is a linear filter that removes DC bias from a signal. Specifically, this is a one-pole highpass filter implementing the formula 
@racketblock[ y[n] = x[n] - x[n-1] + coef * y[n-1] ::. The frequency response of this filter is dependent on the sample rate of the server and the calculation rate of the UGen.

]
@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 in

The input signal.


@section{argument}
 coef

Leak coefficient.


@section{argument}
 mul

Output will be multiplied by this value.


@section{argument}
 add

This value will be added to the output.


@section{Examples}
 


@racketblock[
(
{
	var a;
	a = LFPulse.ar(800, 0.5, 0.5, 0.5);
	[a, LeakDC.ar(a, 0.995)]
}.scope(bufsize: 22050)
)
::

]


