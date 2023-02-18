#lang scribble/manual
@(require (for-label racket))

@title{PeakFollower}
 Track peak signal amplitude.@section{related}
  Classes/Peak
@section{categories}
   UGens>Analysis>Amplitude


@section{description}


Outputs the peak amplitude of the signal received at the input. If level
is below maximum, the level decreases by the factor given in

@racketblock[decay:: .


Internally, the absolute value of the signal is used, to prevent
underreporting the peak value if there is a negative DC offset. To obtain
the minimum and maximum values of the signal as is, use the
link::Classes/RunningMin::  and  link::Classes/RunningMax::  UGens.


]
@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 in

The input signal.


@section{argument}
 decay

Decay factor.


@section{Examples}
 


@racketblock[

s.boot;

// no decay
(
{
	SinOsc.ar(
			PeakFollower.ar(Dust.ar(20, Line.kr(0, 1, 4)), 1.0) * 1500 + 200,
			0, 0.2
	)

}.play;
)

// a little decay
(
{
	SinOsc.ar(
			PeakFollower.ar(Dust.ar(20, Line.kr(0, 1, 4)), 0.999) * 1500 + 200,
			0, 0.2
	)

}.play;
)

// mouse x controls decay, center of the
(
{
	var decay;
	decay = MouseX.kr(0.99, 1.00001).min(1.0);
	SinOsc.ar(
			PeakFollower.ar(Dust.ar(20), decay) * 1500 + 200,
			0, 0.2
	);

}.play;
)




// follow a sine lfo, decay controlled by mouse x
(
{
	var decay;
	decay = MouseX.kr(0, 1.1).min(1.0);
	SinOsc.ar(
			PeakFollower.kr(SinOsc.kr(0.2), decay) * 200 + 500,
			0, 0.2
	)

}.play;
)

::

]


