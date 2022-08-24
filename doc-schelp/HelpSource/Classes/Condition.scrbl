#lang scribble/manual
@(require (for-label racket))

@title{Condition}
@section{categories}
 Scheduling
Block the execution of a thread
@section{CLASSMETHODS}
 

@section{method}
 new
Create a new instance, set the strong::test:: variable.

@section{INSTANCEMETHODS}
 

@section{method}
 test
Answer whether the condition will block or not (boolean).

@section{method}
 wait
Wait until the condition is true and signalled. This only works in a Routine. This method yields a symbol (\hang), so that the clock doesn't reschedule the Routine.

@racketblock[
c = Condition(false); fork { 0.5.wait; "started ...".postln; c.wait;  "... and finished.".postln };
c.test = true;
c.signal;
::

]
@section{method}
 hang
Wait for strong::value:: time, regardless of test. This only works in a Routine. This method yields a symbol (\hang), so that the clock doesn't reschedule the Routine.

@racketblock[
c = Condition.new; fork { 0.5.wait; "started ...".postln; c.hang;  "... and finished.".postln };
c.unhang;
::

]
@section{method}
 signal
If link::#-test:: is true, reschedule blocked threads.

@section{method}
 unhang
Resume threads.

@section{EXAMPLES}
 


@racketblock[
(
c = Condition.new(false);

Routine {
	1.wait;
	"waited for 1 second".postln;
	1.wait;
	"waited for another second, now waiting for you ... ".postln;
	c.wait;
	"the condition has stopped waiting.".postln;
	1.wait;
	"waited for another second".postln;
	"waiting for you ... ".postln;
		c.test = false;
		c.wait;
	"the condition has stopped waiting.".postln;
	1.wait;
	"the end".postln;
}.play;
)

// continue
(
c.test = true;
c.signal;
)

// a typical use is a routine that can pause under certain conditions:
(
c = Condition.new;
fork { loop { 1.wait; "going".postln; c.wait } };
)
c.test = true; c.signal;
c.test = false;
::

]

@racketblock[
// the same, using hang

(
c = Condition.new;

Routine {
	1.wait;
	"waited for 1 second".postln;
	1.wait;
	"waited for another second, now waiting for you ... ".postln;
	c.hang;
	"the condition has stopped waiting.".postln;
	1.wait;
	"waited for another second".postln;
	"waiting for you ... ".postln;
	c.hang;
	"the condition has stopped waiting.".postln;
}.play;
)

// continue
c.unhang;
::

Waiting for Synths to end (waitForFree) uses a Condition implicitly:
]

@racketblock[
(
SynthDef(\help, {
	var mod = LFNoise2.kr(ExpRand(0.5, 2)) * 0.5;
	var snd =  mod * Blip.ar(Rand(200, 800) * (mod + 1));
	Out.ar(0, snd);
	FreeSelf.kr(mod < 0); // free the synth when amplitude goes below 0.
}).add;
)

(
fork {
	10.do {
		"started a synth".postln;
		Synth(\help).waitForFree;
		"This one ended. Wait a second,  I will start the next one.".postln;
		1.wait;
	};
	"This is it.".postln;
}
);
]


