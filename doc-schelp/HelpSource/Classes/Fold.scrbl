#lang scribble/manual
@(require (for-label racket))

@title{Fold}
 Fold a signal outside given thresholds.@section{related}
  Classes/Clip, Classes/Wrap
@section{categories}
   UGens>Maths


@section{description}


This differs from the  link::Classes/BinaryOpUGen::  link::Overviews/Methods#fold2#fold2:: in that it
allows one to set low and high thresholds.


@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 in
Signal to be folded.

@section{argument}
 lo
Low threshold of folding. Sample values < lo will be folded. must be less then hi.

@section{argument}
 hi
High threshold of folding. Sample values > hi will be folded. must be greater then lo.


@section{Examples}
 


@racketblock[
s.boot;

{ Fold.ar(SinOsc.ar(440, 0, 0.2), -0.1, 0.1) }.scope;
::

]


