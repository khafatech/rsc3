#lang scribble/manual
@(require (for-label racket))

@title{T2K}
 Audio rate trigger to control rate trigger converter@section{categories}
  UGens>Conversion, UGens>Triggers
@section{related}
  Classes/T2A, Classes/K2A, Classes/A2K

@section{description}

Converts audio rate trigger into control rate trigger, using the maximum trigger in the input during each control period.

@section{classmethods}
 
@section{method}
  kr

@section{argument}
  in
input signal.

@section{examples}
 

@racketblock[
// this does not work:
(
{
	var trig = Dust.ar(4);
	Trig.kr(trig, 0.1) * SinOsc.ar(800) * 0.1
}.play;
)

// this works:
(
{
	var trig = T2K.kr(Dust.ar(4));
	Trig.kr(trig, 0.1) * SinOsc.ar(800) * 0.1
}.play;
)
::
]


