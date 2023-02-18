#lang scribble/manual
@(require (for-label racket))

@title{EnvGate}
 singleton fade envelope@section{categories}
  Libraries>JITLib>NodeProxy
@section{related}
  Classes/EnvGen

@section{description}

Convenience class for an envelope generator combining fadeTime and gate arguments.

@section{ClassMethods}
 

@section{method}
 new
Returns an link::Classes/EnvGen::.

@section{argument}
 i_level
initial level of envelope (if set to 1, it starts open)

@section{argument}
 gate
a gate input. if nil, EnvGate creates a link::Classes/NamedControl:: named 'gate'

@section{argument}
 fadeTime
an input for both attack and decay time. if nil, EnvGate creates a link::Classes/NamedControl:: named 'fadeTime' (default time: 0.02)

@section{argument}
 doneAction
doneAction of the link::Classes/EnvGen::

@section{argument}
 curve
envelope curve

@section{Examples}
 


@racketblock[
a = { LPF.ar(Saw.ar(200), 600) * EnvGate.new }.play;
a.set(\fadeTime, 2);
a.release;

// the same as:
a.set(\gate, 0);

// several env gates can coexist in one synth def.
(
a = {
	var sound1 = LPF.ar(Saw.ar(80), 600) * EnvGate.new;
	var sound2 = RLPF.ar(Saw.ar(200) * 0.5, 6000 * EnvGate.new + 60, 0.1) * EnvGate.new;
	sound1 + sound2
}.play;
)
a.set(\fadeTime, 5);
a.release;
::
]


