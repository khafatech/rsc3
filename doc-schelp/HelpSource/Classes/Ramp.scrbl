#lang scribble/manual
@(require (for-label racket))

@title{Ramp}
 Break a continuous signal into line segments@section{related}
  Classes/Lag, Classes/VarLag, Classes/Slew
@section{categories}
   UGens>Filters>Linear


@section{description}


Break a continuous signal into linearly interpolated segments with specific durations.

Feeding Ramp with noise is similar to link::Classes/LFNoise1::

@racketblock[
Ramp.kr(WhiteNoise.kr(1),0.5)
::
is equal to:
]

@racketblock[
LFNoise1.kr(1 / 0.5)
::

For smoothing out control signals, take a look at link::Classes/Lag:: and link::Classes/VarLag::

]
@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 in

The input signal.


@section{argument}
 lagTime

segment duration in seconds.


@section{argument}
 mul

Output will be multiplied by this value.


@section{argument}
 add

This value will be added to the output.


@section{Examples}
 


@racketblock[
s.boot;
(
// used to lag pitch
{
	SinOsc.ar(		// sine wave
		Ramp.kr(			// lag the modulator
			LFPulse.kr(4, 0, 0.5, 50, 400),	// frequency modulator
			Line.kr(0, 1, 15)				// modulate lag time
		),
		0,	// phase
		0.3	// sine amplitude
	)
}.scope;
)

// Compare
(
var pulse;
{
	pulse = LFPulse.kr(8.772);
	Out.kr(0,[Ramp.kr(pulse, 0.025), Lag.kr(pulse, 0.025), pulse]);
}.play;
s.scope(3, bufsize: 44100, rate: \control, zoom: 40);
)
::

]


