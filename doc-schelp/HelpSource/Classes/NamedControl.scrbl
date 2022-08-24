#lang scribble/manual
@(require (for-label racket))

@title{NamedControl}
@section{categories}
  UGens>Synth control
 Named reference to a control@section{related}
  Classes/ControlName, Classes/Control

@section{description}


A NamedControl directly combines a ControlName and a Control UGen conveniently. Also this makes it safe even if several identical controls exist (see example below).

There are syntax shortcuts that generate NamedControls from the name:

@racketblock[
\name.ar(values, lags)
\name.kr(values, lags, fixedLag)
\name.ir(values, lags)
\name.tr(values, lags)
::

]
@section{ClassMethods}
 
@section{method}
  ar
add a new instance of link::Classes/AudioControl:: with given name and default values.
If lags are given, apply a Lag UGen to it.
@section{discussion}
 

@racketblock[\symbol.ar(values, lags):: is a synonym.

]
@section{method}
  kr
add a new instance of link::Classes/Control:: (kr) with given name and default values.
If lags are given, apply a link::Classes/Lag:: UGen to it. If fixedLag is set to true, create a link::Classes/LagControl::
(lags cannot be modulated then, but fewer UGens are required).
@section{discussion}
 

@racketblock[\symbol.kr(values, lags, fixedLag):: is a synonym.

]
@section{method}
  ir
add a new instance of link::Classes/Control:: (ir) with given name and default values.
If lags are given, apply a link::Classes/Lag:: UGen to it.
@section{discussion}
 

@racketblock[\symbol.ir(values, lags):: is a synonym.

]
@section{method}
  tr
add a new instance of link::Classes/TrigControl:: with given name and default values.
If lags are given, apply a link::Classes/Lag:: UGen to it.
@section{discussion}
 

@racketblock[\symbol.tr(values, lags):: is a synonym.

]
@section{method}
  new
add a new instance with the given rate, name and default values.
If lags are given, apply a link::Classes/Lag:: UGen to it. If fixedLag is set to true, create a link::Classes/LagControl::
(lags cannot be modulated then, but fewer UGens are required).

@section{Examples}
 

@racketblock[
// use NamedControl to create a number of multichannel controls:

a = { SinOsc.ar(NamedControl.kr(\freq, [300, 330, 370], [1, 0.3, 0.02])).sum * 0.1 }.play;
a.setn(\freq, [700, 705, 890]);
a.setn(\freq, [0, 2, 5].midiratio * 400);

// synonymous:
a = { SinOsc.ar(\freq.kr([300, 330, 370], [1, 0.3, 0.02])).sum * 0.1 }.play;

// multiple usage of the same name:
a = { SinOsc.ar(\freq.kr(440, 3.5)) + Saw.ar(\freq.kr(440, 0.05) * 0.5) * 0.1 }.play;


a.set(\freq, 1220)
a.set(\freq, 120)
::

]
@section{subsection}
  Comparison with direct use of Controls

In the situation when functions are used to combine UGens to more complex SynthDefs, it may not be known which ControlNames are already taken by others. NamedControl allows to reuse existing control names.

@racketblock[
// compare:
(
a = {
	var x, y;
	x = NamedControl.kr(\freq, 440, 3.5);
	y = NamedControl.kr(\freq, 440, 1);
	SinOsc.ar([x, y] * [2, 1.2]) * 0.1
}.play;
)

a.set(\freq, 1220)
a.set(\freq, 120)

(
a = {
	var x, y;
	x = Control.names([\freq]).kr(440).lag(3.5);
	y = Control.names([\freq]).kr(440).lag(1);
	SinOsc.ar([x, y] * [2, 1.2]) * 0.1
}.play;
)

a.set(\freq, 1220)
a.set(\freq, 120)
::

]
@section{subsection}
  Using dictionary with functions to build SynthDefs
Here is a basic example using a dictionary with functions that can be combined to build SynthDefs.

@racketblock[
(
q = ();
q.makeEnv = { |q, env, doneAction = 0| EnvGen.kr(env, NamedControl.kr(\gate, 1), doneAction: doneAction) };
q.chooseNoise = { [ {PinkNoise.ar}, {WhiteNoise.ar}, {LFNoise2.ar(Rand(100, 1000))}].choose.value};
q.filterInput = { |q, in| [
	{ BPF.ar(in * 15, NamedControl.kr(\freq, 800), 0.2) },
	{ RHPF.ar(in, NamedControl.kr(\freq, 800, 0.2), 0.2) }
	].choose.value
};
)

// test the envelope:
a = { SinOsc.ar(440) * q.makeEnv(Env.asr, 2) * 0.1 }.play;
a.set(\gate, -3); // release in 3 seconds

// single channel:
a = { q.chooseNoise * q.makeEnv(Env.asr, 2) }.play;
a.set(\gate, -3); // release in 3 seconds

a = { q.filterInput(q.chooseNoise) * q.makeEnv(Env.asr, 2) }.play;
a.set(\freq, 1000); // set filter frequency
a.set(\gate, -3); // release in 3 seconds

(
a = {
	var channels = Array.fill(8, {
		q.filterInput(q.chooseNoise) * q.makeEnv(Env.asr, 2)
	});
	Splay.ar(channels);

}.play;
)
a.set(\freq, 6000); // set filter frequency
a.set(\gate, -3); // release in 3 seconds
::

]


