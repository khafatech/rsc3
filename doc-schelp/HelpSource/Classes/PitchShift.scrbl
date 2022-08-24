#lang scribble/manual
@(require (for-label racket))

@title{PitchShift}
 Time domain pitch shifter.@section{categories}
   UGens>Filters>Pitch


@section{description}


A time domain granular pitch shifter. Grains have a triangular amplitude
envelope and an overlap of 4:1.


@section{classmethods}
 

@section{method}
 ar

@section{argument}
 in

The input signal.


@section{argument}
 windowSize

The size of the grain window in seconds. This value cannot be
modulated.


@section{argument}
 pitchRatio

The ratio of the pitch shift. Must be from 0 to 4.


@section{argument}
 pitchDispersion

The maximum random deviation of the pitch from the


@racketblock[pitchRatio:: .


]
@section{argument}
 timeDispersion

A random offset of from zero to


@racketblock[timeDispersion::  seconds is added to the
delay of each grain. Use of some dispersion can alleviate a hard
comb filter effect due to uniform grain placement. It can also be
an effect in itself.
]

@racketblock[timeDispersion::  can be
no larger than
]

@racketblock[windowSize:: .


]
@section{argument}
 mul

Output will be multiplied by this value.


@section{argument}
 add

This value will be added to the output.


@section{Examples}
 


@racketblock[

(
play({
	z = Blip.ar(800, 6, 0.1);
	PitchShift.ar(z, 0.02, Line.kr(0.1,4,20), 0, 0.0001)
}))

(
// pitch shift input - USE HEADPHONES to prevent feedback.
play({
	PitchShift.ar(
		SoundIn.ar([0, 1]),	// stereo audio input
		0.1, 			// grain size
		MouseX.kr(0,2),	// mouse x controls pitch shift ratio
		0, 				// pitch dispersion
		0.004			// time dispersion
	)
}))

(
// use PitchShift to granulate input - USE HEADPHONES to prevent feedback.
// upper left corner is normal playback. x = pitch dispersion, y = time dispersion
var grainSize;
grainSize = 0.5;
play({
	PitchShift.ar(
		SoundIn.ar([0, 1]),
		grainSize,
		1,						// nominal pitch rate = 1
		MouseX.kr(0,1), 			// pitch dispersion
		MouseY.kr(0, grainSize)	// time dispersion
	)
}))

::
]


