#lang scribble/manual
@(require (for-label racket))

@title{Timer}
 Returns time since last triggered.@section{categories}
   UGens>Triggers

@section{description}

When triggered, Timer measures the time (in seconds) elapsed since the previous trigger, and outputs this time value as a constant. Its output will not change until the next trigger. The initial value is 0.

If you need the time since the last trigger, where the time is continually updated, see link::Classes/Sweep::.

@section{classmethods}
 
@section{private}
  categories

@section{method}
 ar, kr

@section{argument}
 trig

A trigger occurs when trig signal crosses from non-positive to positive.


@section{Examples}
 


@racketblock[

// using timer to modulate sine frequency: the slower the trigger is the higher the frequency
(
{ var trig;
	trig = Impulse.kr(MouseX.kr(0.5, 20, 1));
	SinOsc.ar(Timer.kr(trig) * 500 + 500, 0, 0.2)
}.play;
)

::

]


