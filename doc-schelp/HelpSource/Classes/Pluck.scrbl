#lang scribble/manual
@(require (for-label racket))

@title{Pluck}
 A Karplus-Strong UGen@section{categories}
  UGens>Delays

@section{description}

A Karplus-Strong UGen

@section{classmethods}
 
@section{method}
  ar

@section{argument}
  in
an excitation signal.
@section{argument}
  trig
upon a negative to positive transition, emphasis::n:: samples of the excitation signal will be fed into the delay line, where emphasis::n = delaytime * sample_rate / 2::, using a rectangular envelope (no fading).
@section{argument}
  maxdelaytime
the max delay time in seconds (initializes the internal delay buffer).
@section{argument}
  delaytime
delay time in seconds.
@section{argument}
  decaytime
time for the echoes to decay by 60 decibels. Negative times emphasize odd partials.
@section{argument}
  coef
the coef of the internal OnePole filter. Values should be between -1 and +1 (larger values will be unstable... so be careful!).
@section{argument}
  mul
@section{argument}
  add

@section{examples}
 

@racketblock[
s.boot;

// excitation signal is WhiteNoise, triggered twice a second with varying OnePole coef
(
	{Pluck.ar(WhiteNoise.ar(0.1), Impulse.kr(2), 440.reciprocal, 440.reciprocal, 10,
		coef:MouseX.kr(-0.999, 0.999))
	}.play(s)
)
s.quit;
// a group of angry fretless mandolin players
(
	{
		var freq, numparts;
		numparts = 50;
		freq = SinOsc.kr(Array.fill(numparts, {Rand(0.05, 0.2)}),
			Array.fill(numparts, {Rand(0, 1.0)})).range(1000, 3000);
		LeakDC.ar(
			Pan2.ar(
				Pluck.ar(
					WhiteNoise.ar(0.1).dup(numparts),
					Impulse.kr(Array.fill(numparts, {Rand(10, 12)})),
					100.reciprocal, freq.reciprocal, 2, Rand(0.01, 0.2), mul: 1),
				Array.fill(numparts, {Rand.new(-1.0, 1.0)}))
			.sum
			);
		}.play(s);
)
::
]


