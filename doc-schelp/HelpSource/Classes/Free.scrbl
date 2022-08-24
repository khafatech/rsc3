#lang scribble/manual
@(require (for-label racket))

@title{Free}
 When triggered, frees a node.@section{related}
  Classes/Pause, Classes/FreeSelf
@section{categories}
   UGens>Synth control

@section{description}

When triggered, frees a node.

@section{classmethods}
 

@section{method}
 kr

@section{argument}
 trig
Trigger input

@section{argument}
 id
Node to be freed.


@section{Examples}
 


@racketblock[
s.boot;

SynthDef("a", { Out.ar(0, SinOsc.ar(800, 0, 0.2)) }).add;

SynthDef("b", { arg t_t=0; Out.ar(1, PinkNoise.ar(0.3)); Free.kr(t_t, 1001); }).add;

s.sendMsg(\s_new, \a, 1001, 0, 0);

s.sendMsg(\s_new, \b, 1002, 0, 0);

s.sendMsg(\n_set, 1002, \t_t, 1);

s.sendMsg(\s_new, \a, 1001, 0, 0);

s.sendMsg(\n_set, 1002, \t_t, 1);

s.sendMsg(\s_new, \a, 1001, 0, 0);

s.sendMsg(\n_set, 1002, \t_t, 1);
::

]


