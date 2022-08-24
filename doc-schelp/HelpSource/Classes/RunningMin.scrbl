#lang scribble/manual
@(require (for-label racket))

@title{RunningMin}
 Track minimum level.@section{related}
  Classes/RunningMax, Classes/RunningSum
@section{categories}
   UGens>Maths


@section{description}


Outputs the minimum value received at the input. When a trigger occurs at
the reset input, the minimum output value is reset to the current value.


@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 in

The input signal.


@section{argument}
 trig

Resets the output value to the current input value. A trigger happens when the signal changes from non-positive to positive.


@section{Examples}
 


@racketblock[

(
{
	SinOsc.ar(
			RunningMin.ar(Dust.ar(20), Impulse.ar(0.4)) * 500 + 200,
			0, 0.2
	)

}.play;
)

// follow a sine lfo, reset rate controlled by mouse x
(
{
	SinOsc.ar(
			RunningMin.kr(SinOsc.kr(0.2), Impulse.kr(MouseX.kr(0.01, 2, 1))) * 500 + 200,
			0, 0.2
	)

}.play;
)

::

]


