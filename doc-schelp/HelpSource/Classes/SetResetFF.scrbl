#lang scribble/manual
@(require (for-label racket))

@title{SetResetFF}
 Set-reset flip flop.@section{related}
  Classes/ToggleFF
@section{categories}
   UGens>Triggers


@section{description}


Output is set to 1.0 upon receiving a trigger in the trig input, and to
0.0 upon receiving a trigger in the reset input. Once the flip flop is
set to zero or one further triggers in the same input are have no effect.
One use of this is to have some precipitating event cause something to
happen until you reset it.

If both inputs receive a trigger at the same time, the teletype::reset:: input takes precedence. The output will be 0. See the examples below.

@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 trig

The trigger that sets output to 1. Trigger can be any signal.
A trigger happens when the signal changes from non-positive to
positive.


@section{argument}
 reset

The trigger that sets output to 0. Trigger can be any signal.
A trigger happens when the signal changes from non-positive to
positive.


@section{Examples}
 


@racketblock[

(
play({
	a = Dust.ar(5); // the set trigger
	b = Dust.ar(5); // the reset trigger
	SetResetFF.ar(a,b) * BrownNoise.ar(0.2);

}))

::

]
@section{subsection}
 Simultaneous triggers

Here, 'reset' is triggered twice as often as 'trig'. Since 'trig' is always matched by a 'reset', the output is 0. (If 'trig' took precedence, you would have a 50%-duty-cycle pulse wave.)


@racketblock[
a = { SetResetFF.kr(Impulse.kr(50), Impulse.kr(100)) }.plot(duration: 0.1);
::

You can reverse this behavior, by reversing the inputs so that SetResetFF is triggered twice as often as resetting. This results in a signal that is 0 initially and switches to 1 halfway through the cycle: emphasis::reset, then trigger::. To make it emphasis::trigger, then reset::, invert the phase: ]

@racketblock[1 - SetResetFF::.

]

@racketblock[
a = { 1 - SetResetFF.kr(Impulse.kr(100), Impulse.kr(50)) }.plot(duration: 0.1);
::

]


