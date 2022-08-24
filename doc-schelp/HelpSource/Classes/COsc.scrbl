#lang scribble/manual
@(require (for-label racket))

@title{COsc}
 Chorusing wavetable oscillator.@section{related}
  Classes/Osc, Classes/OscN, Classes/VOsc, Classes/VOsc3
@section{categories}
   UGens>Generators>Deterministic


@section{description}


Chorusing wavetable lookup oscillator. Produces sum of two signals at


@racketblock[
(freq ± (beats / 2)).
::

Due to summing, the peak amplitude is not the same as the wavetable and can be twice of that.


]
@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 bufnum

The number of a buffer filled in wavetable format.


@section{argument}
 freq

Frequency in Hertz.


@section{argument}
 beats

Beat frequency in Hertz.


@section{argument}
 mul

Output will be multiplied by this value.


@section{argument}
 add

This value will be added to the output.


@section{Examples}
 


@racketblock[

(
b = Buffer.alloc(s, 512, 1, {arg buf; buf.sine1Msg(1.0/[1,2,3,4,5,6,7,8,9,10])});
{ COsc.ar(b.bufnum, 200, 0.7, 0.25) }.play;
)

::

]


