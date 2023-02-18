#lang scribble/manual
@(require (for-label racket))

@title{Saw}
 Band limited sawtooth.@section{related}
  Classes/SyncSaw, Classes/VarSaw, Classes/LFSaw
@section{categories}
   UGens>Generators>Deterministic


@section{description}


Band limited sawtooth wave generator.


@section{classmethods}
 

@section{method}
 ar

@section{argument}
 freq

Frequency in Hertz.


@section{argument}
 mul

Output will be multiplied by this value.


@section{argument}
 add

This value will be added to the output.


@section{Examples}
 


@racketblock[

// modulating the frequency

{ Saw.ar(XLine.kr(40,4000,6),0.2) }.play;

// two band limited sawtooth waves through a resonant low pass filter

{ RLPF.ar(Saw.ar([100,250],0.1), XLine.kr(8000,400,5), 0.05) }.play;

::

]


