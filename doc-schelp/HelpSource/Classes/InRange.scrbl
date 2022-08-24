#lang scribble/manual
@(require (for-label racket))

@title{InRange}
 Tests if a signal is within a given range.@section{related}
  Classes/InRect, Classes/Schmidt
@section{categories}
   UGens>Maths


@section{description}


If  
@racketblock[in::  is ≥  ]

@racketblock[lo::  and ≤
]

@racketblock[hi::  output 1.0, otherwise output 0.0. Output is
initially zero.


]
@section{classmethods}
 

@section{method}
 ar, kr, ir

@section{argument}
 in

Signal to be tested.


@section{argument}
 lo

Low threshold.


@section{argument}
 hi

High threshold.


@section{Examples}
 


@racketblock[

s.boot;

{ InRange.kr(SinOsc.kr(1, 0, 0.2), -0.15, 0.15)}.scope; // see the trigger

{ InRange.kr(SinOsc.kr(1, 0, 0.2), -0.15, 0.15) * BrownNoise.ar(0.1)}.scope; // trigger noise Burst

::

]


