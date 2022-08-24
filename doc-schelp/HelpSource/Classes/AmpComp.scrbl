#lang scribble/manual
@(require (for-label racket))

@title{AmpComp}
 Basic psychoacoustic amplitude compensation.@section{related}
  Classes/AmpCompA
@section{categories}
   UGens>Analysis>Amplitude


@section{description}


Implements the (optimized) formula:


@racketblock[
compensationFactor = (root / freq) ** exp
::


Higher frequencies are normally perceived as louder, which AmpComp
compensates.


]
@section{classmethods}
 

@section{method}
 ar, kr, ir

@section{argument}
 freq

Input frequency value. For freq == root, the output is 1.0.


@section{argument}
 root

Root freq relative to which the curve is calculated
(usually lowest freq).


@section{argument}
 exp

Exponent: how steep the curve decreases for increasing freq.

@section{discussion}
 
Note that for frequencies very much smaller than root the amplitudes can become very high.
In this case limit the freq with 
@racketblock[freq.max(minval)::, or use AmpCompA.

]
@section{Examples}
 


@racketblock[
// compare a sine without compensation

{ SinOsc.ar(MouseX.kr(300, 15000, 1)) * 0.1 }.play;

// with one that uses amplitude compensation
(
{
	var freq;
	freq = MouseX.kr(300, 15000, 1);
	SinOsc.ar(freq) * 0.1 * AmpComp.kr(freq, 300)
}.play;
)


// different sounds cause quite different loudness perception,
// and the desired musical behavior can vary, so the exponent can be tuned:
(
{
	var freq;
	freq = MouseX.kr(300, 15000, 1);
	Pulse.ar(freq) * 0.1 * AmpComp.kr(freq, 300, 1.3)
}.play;
)

// the curves:

// exp = 0.3333
(200,210..10000).collect {|freq| (200/freq) ** 0.3333 }.plot;

// nearly linear for semitone steps:

(48..72).midicps.collect {|freq| (48.midicps/freq) ** 0.3333 }.plot;
{ AmpComp.ar(Line.ar(48, 72, 1).midicps, 48.midicps) }.plot(1.0);

// exp = 1.2
(200,210..10000).collect {|freq| (200/freq) ** 1.2 }.plot;
(48..72).midicps.collect {|freq| (200/freq) ** 1.2 }.plot;
{ AmpComp.ar(Line.ar(48, 72, 1).midicps, 48.midicps, 1.2) }.plot(1.0);


// amplitude compensation in frequency modulation
(
{
	var freq;
	freq = MouseX.kr(300, 15000, 1);
	freq = freq * SinOsc.ar(MouseY.kr(3, 200, 1), 0, 0.5, 1);
	SinOsc.ar(freq) * 0.1 * AmpComp.ar(freq, 300)
}.play;
)

// without amplitude compensation
(
{
	var freq;
	freq = MouseX.kr(300, 15000, 1);
	freq = freq * SinOsc.ar(MouseY.kr(3, 200, 1), 0, 0.5, 1);
	SinOsc.ar(freq) * 0.1
}.play;
)

// in granular synthesis:
(
SynthDef("pgrain",
	{ arg out = 0, sustain=0.01, amp=0.5, pan = 0;
		var freq = MouseX.kr(300, 7000, 1);
		var window = Env.sine(sustain, amp * AmpComp.ir(freq));
		Out.ar(out,
			Pan2.ar(
				SinOsc.ar(freq),
				pan
			) * EnvGen.ar(window, doneAction: Done.freeSelf)
		)
	}
).add;
)

// send grains
(
fork {
	loop {
		s.sendBundle(0.1, [\s_new, \pgrain, -1,1,1]);
		0.02.wait;
	};
}
)


// try different synth defs:


// without AmpComp:

(
SynthDef("pgrain",
	{ arg out = 0, sustain=0.01, amp=0.5, pan = 0;
		var freq = MouseX.kr(300, 7000, 1);
		var window = Env.sine(sustain, amp);
		Out.ar(out,
			Pan2.ar(
				SinOsc.ar(freq),
				pan
			) * EnvGen.ar(window, doneAction: Done.freeSelf)
		)
	}
).add;
)

// with AmpCompA
(
SynthDef("pgrain",
	{ arg out = 0, sustain=0.01, amp=0.5, pan = 0;
		var freq = MouseX.kr(300, 7000, 1);
		var window = Env.sine(sustain, amp * AmpCompA.ir(freq));
		Out.ar(out,
			Pan2.ar(
				SinOsc.ar(freq),
				pan
			) * EnvGen.ar(window, doneAction: Done.freeSelf)
		)
	}
).add;
)
::

]


