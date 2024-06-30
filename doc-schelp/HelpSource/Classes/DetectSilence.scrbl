#lang scribble/manual
@(require (for-label racket))

@title{DetectSilence}
 Detect when input falls below an amplitude threshold@section{categories}
   UGens>Synth control, UGens>Analysis>Amplitude


@section{description}


When the absolute value of the input signal remains below the threshold for a given window of time, output 1. Otherwise, output 0. If the output transitions from 0 to 1, doneAction is also evaluated.


@racketblock[
// this frees after the Decay has become quiet enough for a long enough time
{ var signal = Decay.ar(Impulse.ar(0), 2, PinkNoise.ar(0.2)); DetectSilence.ar(signal, doneAction: Done.freeSelf); signal }.play;
::

If the input signal starts with silence at the beginning of the synth's duration, then DetectSilence will wait indefinitely until the first sound before starting to monitor for silence. To avoid a hanging silent sound where the input may remain zero, you can add an ]

@racketblock[Impulse.ar(0):: to its input.

DetectSilence does not distinguish a DC-biased signal from a loud signal. If your signal has DC bias, you should wrap it in link::Classes/LeakDC::.

]
@section{warning}
  DetectSilence can be tricky with multi-channel input! See below. ::

@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 in

The input signal.


@section{argument}
 amp

When input falls below this for long enough, evaluate


@racketblock[doneAction:: .


]
@section{argument}
 time

The minimum duration for which input must fall below


@racketblock[amp::  before this triggers.


]
@section{argument}
 doneAction

An integer representing the doneAction. See

link::Classes/Done::  for more detail.

@section{returns}
 
This UGen outputs 1 if silence is detected, otherwise 0.

@section{Examples}
 


@racketblock[

(
SynthDef("detectSilence-help", { arg out;
	var z;
	z = SinOsc.ar(Rand(400, 700), 0, LFDNoise3.kr(8).max(0)).softclip * 0.3;
	DetectSilence.ar(z, doneAction: Done.freeSelf);
	Out.ar(out, z);
}).add;
)

Synth("detectSilence-help");
Synth("detectSilence-help");
Synth("detectSilence-help");


(
Task({
	loop({
		Synth("detectSilence-help");
		[0.5, 1].choose.wait;
	})
}).play;
)

::

]
@section{section}
 DetectSilence and multiple inputs

Be careful feeding multiple inputs into DetectSilence, since multichannel expansion can lead to confusing behavior. This code


@racketblock[DetectSilence.ar([left, right] + Impulse.ar(0), doneAction: 2)::

spawns strong::two:: DetectSilences, and the enclosing Synth frees itself when left OR right becomes silent. (The ]

@racketblock[Impulse.ar(0):: is there to ensure that DetectSilence starts detecting silence right away rather than waiting for non-silence.)

Usually you want to detect silence for both inputs, so what's a good way to change that OR into an AND? The solution is strong::not:: to sum ]

@racketblock[(left + right) / 2.sqrt::, because there may be phase cancellation in that sum. Instead, use DetectSilence's output instead of its doneAction, and use FreeSelf to do the freeing. Recall that DetectSilence outputs 1 if silence is detected, and 0 otherwise. Taking the product of multiple DetectSilences will return 1 only if all the DetectSilences are 1. So here is the correct solution to freeing a Synth when both ]

@racketblock[left:: and ]

@racketblock[right:: first fall silent:

]

@racketblock[FreeSelf.kr(DetectSilence.ar([left, right] + Impulse.ar(0)).product)::

It's also possible to mix the signals with full-wave rectification to avert phase cancellation issues (]

@racketblock[[left, right].abs.sum / 2.sqrt::).
]

