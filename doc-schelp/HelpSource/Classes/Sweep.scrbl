#lang scribble/manual
@(require (for-label racket))

@title{Sweep}
 Triggered linear ramp@section{categories}
   UGens>Triggers

@section{description}


Starts a linear raise by rate/sec from zero when trig input crosses from
non-positive to positive.

When rate == 1, Sweep may be used to get a continually-updating measurement
of the time (in seconds) since the last trigger.


@section{classmethods}
 
@section{private}
  categories

@section{method}
 ar, kr

@section{argument}
 trig

triggers when trig input crosses from non-positive to positive.


@section{argument}
 rate

rate/sec raise rate


@section{Examples}
 


@racketblock[
// using sweep to modulate sine frequency
(
{ var trig;
	trig = Impulse.kr(MouseX.kr(0.5, 20, 1));
	SinOsc.ar(Sweep.kr(trig, 700) + 500, 0, 0.2)
}.play;
)


// using sweep to index into a buffer
b = Buffer.read(s, Platform.resourceDir +/+ "sounds/a11wlk01.wav");

(
{ var trig;
	trig = Impulse.kr(MouseX.kr(0.5, 10, 1));
	BufRd.ar(1, b, Sweep.ar(trig, BufSampleRate.ir(0)))
}.play;
)

// backwards, variable offset
(
{ var trig, pos, rate;
	trig = Impulse.kr(MouseX.kr(0.5, 10, 1));
	rate = BufSampleRate.ir(0);
	pos = Sweep.ar(trig, rate.neg) + (BufFrames.ir(0) * LFNoise0.kr(0.2));
	BufRd.ar(1, b, pos)
}.play;
)

// raising rate
(
{ var trig, rate;
	trig = Impulse.kr(MouseX.kr(0.5, 10, 1));
	rate = Sweep.kr(trig, 2) + 0.5;
	BufRd.ar(1, b, Sweep.ar(trig, BufSampleRate.ir(0) * rate))
}.play;
)

b.free

::

Sweep can be used as a resettable link::Classes/Phasor:: or link::Classes/Line:: - one that can start, pause, resume and stop. To get a resettable link::Classes/XLine:: behavior change the ]

@racketblock[linlin:: to ]

@racketblock[linexp:: in the SynthDef below.
]

@racketblock[
(
SynthDef(\lineReset, {|start= 0, end= 1, dur= 1, t_trig= 1, run= 1|
	var phasor= (Sweep.ar(t_trig, 1/dur*run)).linlin(0, 1, start, end, \minmax);
	phasor.poll;
	Out.ar(0, SinOsc.ar(phasor, 0, 0.2));
}).add;
)
a= Synth(\lineReset, [\start, 400, \end, 800, \dur, 2])
a.set(\t_trig, 1)
a.set(\run, 0)
a.set(\run, 1)
a.set(\t_trig, 1)
a.free

//shorter duration and downwards...
a= Synth(\lineReset, [\start, 1000, \end, 500, \dur, 0.5])
a.set(\t_trig, 1)
a.set(\run, 0)
a.set(\run, 1)
a.set(\t_trig, 1)
a.free
::
]


