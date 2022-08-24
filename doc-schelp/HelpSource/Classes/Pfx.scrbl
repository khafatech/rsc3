#lang scribble/manual
@(require (for-label racket))

@title{Pfx}
 add an effect synth to the synths of a given event stream@section{related}
  Classes/Pfxb, Classes/Pbus, Classes/Pgroup
@section{categories}
  Streams-Patterns-Events>Patterns>Server Control

@section{description}


Puts an effect node on the tail of the current group and releases it when the contained pattern finishes. If a bus is given, it is used as an effect bus. Name value pairs are inserted into the event for starting the effect node. The effect parameters are set from the event.

@section{Examples}
 


@racketblock[
(
SynthDef(\echo, { arg out=0, maxdtime=0.2, dtime=0.2, decay=2, gate=1;
	var env, in;
	env = Linen.kr(gate, 0.05, 1, 0.1, 2);
	in = In.ar(out, 2);
	XOut.ar(out, env, CombL.ar(in * env, maxdtime, dtime, decay, 1, in));
}, [\ir, \ir, 0.1, 0.1, 0]).add;

SynthDef(\distort, { arg out=0, pregain=40, amp=0.2, gate=1;
	var env;
	env = Linen.kr(gate, 0.05, 1, 0.1, 2);
	XOut.ar(out, env, (In.ar(out, 2) * pregain).distort * amp);
}, [\ir, 0.1, 0.1, 0]).add;

SynthDef(\wah, { arg out=0, gate=1;
	var env, in;
	env = Linen.kr(gate, 0.05, 1, 0.4, 2);
	in = In.ar(out, 2);
	XOut.ar(out, env, RLPF.ar(in, LinExp.kr(LFNoise1.kr(0.3), -1, 1, 200, 8000), 0.1).softclip * 0.8);
}, [\ir, 0]).add;
)

(
var p, q, r, o;
p = Pbind(\degree, Prand((0..7),12), \dur, 0.3, \legato, 0.2);

q = Pfx(p, \echo, \dtime, 0.2, \decay, 3);

r = Pfx(q, \distort, \pregain, 20, \amp, 0.25);

o = Pfx(r, \wah);

Pseq([p, q, r, o], 2).play;
)
::
]


