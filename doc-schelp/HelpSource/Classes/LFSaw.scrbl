#lang scribble/manual
@(require (for-label racket))

@title{LFSaw}
 Sawtooth oscillator@section{related}
  Classes/LFCub, Classes/LFPar, Classes/LFPulse, Classes/LFTri, Classes/Saw
@section{categories}
   UGens>Generators>Deterministic


@section{description}


A non-band-limited sawtooth oscillator. Output ranges from -1 to +1.


@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 freq
Frequency in Hertz.

@section{argument}
 iphase

Initial phase offset. For efficiency reasons this is a value
ranging from 0 to 2.
@section{Note}
  enter "1" for an initial phase of 0 radiants.
A value of 0 would start the cycle at pi radiants. See the example below.::

@section{argument}
 mul
Output will be multiplied by this value.

@section{argument}
 add
This value will be added to the output.

@section{Examples}
 


@racketblock[
{ LFSaw.ar(500, 1, 0.1) }.play

// used as both Oscillator and LFO:
{ LFSaw.ar(LFSaw.kr(4, 0, 200, 400), 0, 0.1) }.play

::
Display special behaviour of initial phase parameter:
]

@racketblock[
// 0.15 seconds of 3-channel buffer
b = Buffer.alloc(s, s.sampleRate * 0.15, 3);

(
// generate Signal
m = SynthDef ("help-lfsawphase", {|rate = 20|
	var l;
	l = LFSaw.ar(rate, [0, 1, 2]); // three channels, three phases
	RecordBuf.ar(l, b, doneAction: Done.freeSelf, loop: 0);
	Out.ar(0, l * -20.dbamp);
}).play(s);
)

b.plot;	// see the difference
::
]


