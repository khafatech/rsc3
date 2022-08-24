#lang scribble/manual
@(require (for-label racket))

@title{Clip}
 Clip a signal outside given thresholds.@section{related}
  Classes/Fold, Classes/Wrap
@section{categories}
   UGens>Maths


@section{description}


This differs from the  link::Classes/BinaryOpUGen::  link::Overviews/Methods#clip2#clip2:: in that it
allows one to set both low and high thresholds.


@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 in

Signal to be clipped.


@section{argument}
 lo

Low threshold of clipping. Must be less then hi.


@section{argument}
 hi

High threshold of clipping. Must be greater then lo.


@section{Examples}
 


@racketblock[

s.boot;

{ Clip.ar(SinOsc.ar(440, 0, 0.2), -0.07, 0.07) }.scope;

::

]


