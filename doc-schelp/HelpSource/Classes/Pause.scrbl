#lang scribble/manual
@(require (for-label racket))

@title{Pause}
 When triggered, pauses a node.@section{related}
  Classes/Free
@section{categories}
   UGens>Synth control


@section{description}


When triggered, pauses a node.


@section{classmethods}
 

@section{method}
 kr

@section{argument}
 gate

When gate is 0, node is paused, when 1 it runs.


@section{argument}
 id

Node to be paused.


@section{Examples}
 


@racketblock[

s.boot;

SynthDef("a", { Out.ar(0, SinOsc.ar(800, 0, 0.2)) }).add;

SynthDef("b", { arg gate=1; Out.ar(1, PinkNoise.ar(0.3)); Pause.kr(gate, 1001); }).add;

s.sendMsg(\s_new, \a, 1001, 0, 0);

s.sendMsg(\s_new, \b, 1002, 0, 0);

s.sendMsg(\n_set, 1002, \gate, 0);

s.sendMsg(\n_set, 1002, \gate, 1);

::

]


