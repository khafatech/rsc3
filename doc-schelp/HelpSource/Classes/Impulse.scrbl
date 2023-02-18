#lang scribble/manual
@(require (for-label racket))

@title{Impulse}
 Impulse oscillator.@section{related}
  Classes/Blip
@section{categories}
   UGens>Generators>Deterministic


@section{description}


Outputs non-bandlimited single sample impulses.


@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 freq

Frequency in Hertz.


@section{argument}
 phase

Phase offset in cycles (0..1).


@section{argument}
 mul

Output will be multiplied by this value.


@section{argument}
 add

This value will be added to the output.

@section{discussion}
 
An Impulse with frequency 0 returns a single impulse.

@section{Examples}
 


@racketblock[
{ Impulse.ar(800, 0.0, 0.5, 0) }.play

{ Impulse.ar(XLine.kr(800,100,5), 0.0,  0.5, 0) }.play
::

modulate phase:
]

@racketblock[
{ Impulse.ar(4, [0, MouseX.kr(0, 1)], 0.2) }.play;
::

an Impulse with frequency 0 returns a single impulse:
]

@racketblock[
SynthDef(\imp, { OffsetOut.ar(0, Impulse.ar(0)); FreeSelf.kr(Impulse.kr(0)); }).add;
fork { (1 / (1..60).scramble).do { |dt| Synth.grain(\imp);  dt.wait } };
::

]


