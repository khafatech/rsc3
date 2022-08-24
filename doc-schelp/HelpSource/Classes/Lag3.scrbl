#lang scribble/manual
@(require (for-label racket))

@title{Lag3}
 Exponential lag@section{related}
  Classes/Lag, Classes/Lag2, Classes/Ramp, Classes/Lag3UD
@section{categories}
   UGens>Filters>Linear


@section{description}


Lag3 is equivalent to 
@racketblock[ Lag.kr(Lag.kr(Lag.kr(in, time), time), time) ::, thus
resulting in a smoother transition. This saves on CPU as you only have to
calculate the decay factor once instead of three times. See
link::Classes/Lag::  for more details.


]
@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 in

The input signal.


@section{argument}
 lagTime

60 dB lag time in seconds.


@section{argument}
 mul

Output will be multiplied by this value.


@section{argument}
 add

This value will be added to the output.


@section{Examples}
 


@racketblock[
(
// used to lag pitch
{
	SinOsc.ar(		// sine wave
		Lag3.kr(			// lag the modulator
			LFPulse.kr(4, 0, 0.5, 50, 400),	// frequency modulator
			Line.kr(0, 1, 15)				// modulate lag time
		),
		0,	// phase
		0.3	// sine amplitude
	)
}.play
)
::

]


