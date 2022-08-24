#lang scribble/manual
@(require (for-label racket))

@title{OneZero}
 One zero filter.@section{related}
  Classes/OnePole
@section{categories}
   UGens>Filters>Linear


@section{description}


A one zero filter. Implements the formula:


@racketblock[

out(i) = ((1 - abs(coef)) * in(i)) + (coef * in(i-1)).

::


]
@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 in

The input signal.


@section{argument}
 coef

Feed forward coefficient.


+0.5 makes a two point averaging filter
(see also
link::Classes/LPZ1:: ).


-0.5 makes a differentiator
(see also
link::Classes/HPZ1:: ).


+1 makes a single sample delay
(see also
link::Classes/Delay1:: ).


-1 makes an inverted single sample delay.


@section{argument}
 mul

Output will be multiplied by this value.


@section{argument}
 add

This value will be added to the output.


@section{Examples}
 


@racketblock[

{ OneZero.ar(WhiteNoise.ar(0.5), 0.5) }.play

{ OneZero.ar(WhiteNoise.ar(0.5), -0.5) }.play

{ OneZero.ar(WhiteNoise.ar(0.5), Line.kr(-0.5, 0.5, 10)) }.play

::

]


