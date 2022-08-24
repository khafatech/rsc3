#lang scribble/manual
@(require (for-label racket))

@title{Wrap}
 Wrap a signal outside given thresholds.@section{related}
  Classes/Clip, Classes/Fold
@section{categories}
   UGens>Maths


@section{description}


This differs from the  link::Classes/BinaryOpUGen::  link::Overviews/Methods#wrap2#wrap2:: in that it
allows one to set both low and high thresholds.


@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 in

Signal to be wrapped.


@section{argument}
 lo

Low threshold of wrapping.


@section{argument}
 hi

High threshold of wrapping.


@section{Examples}
 


@racketblock[

s.boot;

{ Wrap.ar(SinOsc.ar(440, 0, 0.2), -0.15, 0.15) }.scope;

::

]


