#lang scribble/manual
@(require (for-label racket))

@title{TDelay}
 Trigger delay.@section{categories}
   UGens>Triggers, UGens>Delays


@section{description}


Delays a trigger by a given time. Any triggers which arrive in the time
between an input trigger and its delayed output, are ignored.


@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 in

Input trigger signal.


@section{argument}
 dur

Delay time in seconds.


@section{Examples}
 


@racketblock[

(
{
	z = Impulse.ar(2);
	[z * 0.1, ToggleFF.ar(TDelay.ar(z, 0.5)) * SinOsc.ar(mul: 0.1)]
}.scope)

::
]


