#lang scribble/manual
@(require (for-label racket))

@title{Changed}
@section{categories}
  UGens>Triggers, UGens>Filters>Linear
 Triggers when a value changes
@section{description}

Triggers when a value changes.

@section{classmethods}
 
@section{method}
  ar, kr
A special case fixed filter.

@section{argument}
  input
signal input
@section{argument}
  threshold
threshold

@section{discussion}
 
Implements the formula:

@racketblock[
out(i) = abs(in(i) - in(i-1)) > thresh
::

]
@section{examples}
 

detect changes in a signal:

@racketblock[
(
{
	var changingSignal = LFNoise0.ar(1000);
	var changed = Changed.ar(changingSignal);
	[changingSignal, changed]
}.plot2
);
::

]


