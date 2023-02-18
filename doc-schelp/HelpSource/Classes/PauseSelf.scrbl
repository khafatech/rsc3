#lang scribble/manual
@(require (for-label racket))

@title{PauseSelf}
 When triggered, pause enclosing synth.@section{related}
  Classes/FreeSelf
@section{categories}
   UGens>Synth control


@section{description}


Pause enclosing synth when input signal crosses from non-positive to
positive.


@section{classmethods}
 

@section{method}
 kr

@section{argument}
 in

The input signal.


@section{Examples}
 


@racketblock[

(
SynthDef("pauseSelf-help", { arg out, t_trig;
	PauseSelf.kr(t_trig);
	Out.ar(out, SinOsc.ar(400,0,0.2));
}).add;
)

s.sendMsg("/s_new", "pauseSelf-help", 1731);
s.sendMsg("/n_set", 1731, \t_trig, 1);
s.sendMsg("/n_run", 1731, 1);
s.sendMsg("/n_set", 1731, \t_trig, 1);
s.sendMsg("/n_free", 1731);

::

]


