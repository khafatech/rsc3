#lang scribble/manual
@(require (for-label racket))

@title{SyncSaw}
 Hard sync sawtooth wave.@section{related}
  Classes/Saw, Classes/VarSaw, Classes/LFSaw
@section{categories}
   UGens>Generators>Deterministic


@section{description}


A sawtooth wave that is hard synched to a fundamental pitch. This
produces an effect similar to  moving formants or pulse width modulation.
The sawtooth oscillator has its phase reset when the sync oscillator
completes a cycle. This is not a band limited waveform, so it may alias.


@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 syncFreq

Frequency of the fundamental.


@section{argument}
 sawFreq

Frequency of the slave synched sawtooth wave. Should always be
greater than

@racketblock[syncFreq:: .


]
@section{argument}
 mul

Output will be multiplied by this value.


@section{argument}
 add

This value will be added to the output.


@section{Examples}
 


@racketblock[

{ SyncSaw.ar(100, Line.kr(100, 800, 12), 0.1) }.play;


(
plot { [
	SyncSaw.ar(800, 1200),
	Impulse.ar(800) // to show sync rate
] }
)

(
plot { [
	SyncSaw.ar(800, Line.kr(800, 1600, 0.01)), // modulate saw freq
	Impulse.ar(800) // to show sync rate
] }
)

// scoping the saw: hit 's' when focused on the scope window to compare the channels
(
scope {
	var freq = 400;
	[
	SyncSaw.ar(freq, freq * MouseX.kr(1, 3)), // modulate saw freq
	Impulse.ar(freq) // to show sync rate
] * 0.3 }
)

::

]


