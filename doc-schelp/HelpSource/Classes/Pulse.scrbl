#lang scribble/manual
@(require (for-label racket))

@title{Pulse}
 Band limited pulse wave.@section{related}
  Classes/LFPulse
@section{categories}
   UGens>Generators>Deterministic


@section{description}


Band limited pulse wave generator with pulse width modulation.


@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 freq

Frequency in Hertz.


@section{argument}
 width

Pulse width ratio from zero to one. 0.5 makes a square wave.


@section{argument}
 mul

Output will be multiplied by this value.


@section{argument}
 add

This value will be added to the output.


@section{Examples}
 


@racketblock[

// modulate frequency
{ Pulse.ar(XLine.kr(40, 4000, 6), 0.1, 0.2) }.play;

// modulate pulse width
{ Pulse.ar(200, SinOsc.kr(0.2).range(0.01, 0.99), 0.2) }.play;

// two band limited square waves thru a resonant low pass filter
{ RLPF.ar(Pulse.ar([100, 250], 0.5, 0.1), XLine.kr(8000, 400, 5), 0.05) }.play;

::

]


