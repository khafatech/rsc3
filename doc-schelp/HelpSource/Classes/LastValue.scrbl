#lang scribble/manual
@(require (for-label racket))

@title{LastValue}
 Output the last value before the input changed@section{related}
  Classes/LeastChange, Classes/MostChange
@section{categories}
   UGens>Triggers


@section{description}


Output the last value before the input changed more than a threshold.


@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 in

The input signal.


@section{argument}
 diff

Difference threshold.


@section{Examples}
 


@racketblock[
d = { arg freq=440; SinOsc.ar(LastValue.ar(freq, 20), 0, 0.2) }.play;

d.set(\freq, 400);
d.set(\freq, 200);
d.set(\freq, 670);
d.set(\freq, 680);
d.set(\freq, 695);
d.free;
::

Return the difference between current and the last changed:
]

@racketblock[
(
d = { arg out=0, val=1;
	SinOsc.ar(
			abs(val - LastValue.kr(val)) * 400 + 200,
			0, 0.2
	)
}.play;
)

d.set(\val, 3);
d.set(\val, 2);
d.set(\val, 0.2);
d.set(\val, 1);
d.set(\val, 2);
d.free;
::

]


