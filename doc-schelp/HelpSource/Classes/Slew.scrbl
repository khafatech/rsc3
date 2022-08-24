#lang scribble/manual
@(require (for-label racket))

@title{Slew}
 Slew rate limiter.@section{related}
  Classes/Slope, Classes/Ramp, Classes/Lag, Classes/VarLag
@section{categories}
   UGens>Filters>Nonlinear


@section{description}


Limits the slope of an input signal. The slope is expressed in units per
second.

For smoothing out control signals, take a look at link::Classes/Lag:: and link::Classes/VarLag::

@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 in

The input signal.


@section{argument}
 up

Maximum upward slope in units per second.


@section{argument}
 dn

Maximum downward slope in units per second.


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
	z = LFPulse.ar(800);
	[z, Slew.ar(z, 4000, 4000)]
}.plot)



Has the effect of removing transients and higher frequencies.
(
{

	z = Saw.ar(800,mul:0.2);
	Slew.ar(z,400,400)

}.play
)

::

]


