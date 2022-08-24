#lang scribble/manual
@(require (for-label racket))

@title{Peak}
 Track peak signal amplitude.@section{related}
  Classes/PeakFollower
@section{categories}
   UGens>Analysis>Amplitude


@section{description}


Outputs the peak amplitude of the signal received at the input. When
a trigger occurs at the  
@racketblock[reset::  input, the maximum
output value is reset to the current value.


Internally, the absolute value of the signal is used, to prevent
underreporting the peak value if there is a negative DC offset. To obtain
the minimum and maximum values of the signal as is, use the
link::Classes/RunningMin::  and  link::Classes/RunningMax::  UGens.


]
@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 in

The input signal.

@section{argument}
 trig

Trigger. Resets the output value to the current input value. A trigger happens when the signal changes from non-positive to positive.


@section{Examples}
 


@racketblock[

(
{
	SinOsc.ar(
			Peak.ar(Dust.ar(20), Impulse.ar(0.4)) * 500 + 200,
			0, 0.2
	)

}.play;
)

// follow a sine lfo, reset rate controlled by mouse x
(
{
	SinOsc.ar(
			Peak.kr(SinOsc.kr(0.2), Impulse.kr(MouseX.kr(0.01, 2, 1))) * 500 + 200,
			0, 0.2
	)

}.play;
)

::

]


