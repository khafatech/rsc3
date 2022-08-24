#lang scribble/manual
@(require (for-label racket))

@title{15. Sequencing with Routines and Tasks}
 Getting Started With SuperCollider@section{categories}
  Tutorials>Getting-Started
@section{related}
  Tutorials/Getting-Started/00-Getting-Started-With-SC

When you schedule a function (as in the Scheduling Events tutorial), the function always begins at the beginning and runs through to the end. For sequencing, it's more useful to have a control structure that can run part of the way through, return a value, and then pick up where it left off the next time it's needed. In SuperCollider, this is a Routine.

Routines can be used for data processing, e.g.


@racketblock[
r = Routine({
	"abcde".yield;
	"fghij".yield;
	"klmno".yield;
	"pqrst".yield;
	"uvwxy".yield;
	"z{|}~".yield;
});

r.next;	// get the next value from the Routine
6.do({ r.next.postln });
::

The first time you call next, the routine yields strong::"abcde"::. This yield value becomes the result of r.next, and is printed in the post window. On the second next call, execution picks up just after the first yield and continues with the second string, and so forth. When there is nothing more to yield, r.next returns nil.

We will come back to the use of routines for data generation. More important for sequencing is what happens when you schedule a routine on a clock, and the routine returns time values.

]
@section{section}
 Scheduling routines

Recall that, when you schedule a function on a clock, numbers returned by the function are treated as time values -- specifically, the amount of time until the function should execute again. The same thing happens with numbers yielded by a routine.


@racketblock[
r = Routine({
	var delta;
	loop {
		delta = rrand(1, 3) * 0.5;
		"Will wait ".post; delta.postln;
		delta.yield;
	}
});

r.next;

TempoClock.default.sched(0, r);

r.stop;
::

Now let's replace the posting statements with instructions to play a synth. Preparation:

]

@racketblock[
(
SynthDef(\singrain, { |freq = 440, amp = 0.2, sustain = 1|
	var sig;
	sig = SinOsc.ar(freq, 0, amp) * EnvGen.kr(Env.perc(0.01, sustain), doneAction: Done.freeSelf);
	Out.ar(0, sig ! 2);	// sig ! 2 is the same as [sig, sig]
}).add;

r = Routine({
	var delta;
	loop {
		delta = rrand(1, 3) * 0.5;
		Synth(\singrain, [freq: exprand(200, 800), amp: rrand(0.1, 0.5), sustain: delta * 0.8]);
		delta.yield;
	}
});
)
::

Scheduling a routine makes a certain sense, but playing a routine seems more intuitive.

]

@racketblock[
r.play;

r.stop;
::

There you go -- our first sequence.

]
@section{section}
 Pause and resume: Task

Routines have one sticky little characteristic that can limit their usefulness as musical objects. Once you stop a routine, you can only start it over again from the beginning. There is no way to replay the routine from the point where it was stopped.

Task is a variation that can be paused and resumed at will. For example, let's iterate over a C major scale. Note that all of SuperCollider's control structures are valid inside a Routine or Task. Note also that we can use 'wait' as a synonym for 'yield'.


@racketblock[
(
t = Task({
	loop {
		[60, 62, 64, 65, 67, 69, 71, 72].do({ |midi|
			Synth(\singrain, [freq: midi.midicps, amp: 0.2, sustain: 0.1]);
			0.125.wait;
		});
	}
}).play;
)

// probably stops in the middle of the scale
t.stop;

t.play;	// should pick up with the next note

t.stop;
::

Task will be used for the remainder of this tutorial.

]
@section{section}
 When do you want to start?

By default, strong::play:: applied to a Task starts the Task immediately. Most of the time, many tasks will be running simultaneously, and they should be synchronized. While there might be a virtuoso out there somewhere who can hit the enter key at just right time for precise sync, most of us would prefer a more reliable mechanism.

Play takes several arguments to control its behavior.


@racketblock[
aRoutine.play(clock, quant)
aTask.play(argClock, doReset, quant)
::

]
@section{definitionList}
 
## strong::clock:: (Routine) or strong::argClock:: (Task) || Which clock should handle scheduling for this sequence
## strong::doReset:: (Task only) || If true, reset the sequence to the beginning before playing; if false (default), resume
## strong::quant:: || A specification of the exact starting time
::

The quant argument uses a basic model of two numbers, which can be related to the western concept of meter:

quant: Corresponds roughly to bar length; the current time is rounded up to the next multiple of this number
phase: Position within the bar (0 = beginning of the bar)

For convenience, if you just want to start at the beginning of the bar, you can give the bar length as a number. An array of two numbers tells SuperCollider the bar length and the phase.

To see how this works in practice, let's take the C major scale above and play two copies of it slightly offset. We'll slow the rhythm down to 16th-notes (0.25) and start the second one 8th-note into the bar. We will need two tasks to do this, which will be manufactured in a function.


@racketblock[
(
f = {
	Task({
		loop {
			[60, 62, 64, 65, 67, 69, 71, 72].do({ |midi|
				Synth(\singrain, [freq: midi.midicps, amp: 0.2, sustain: 0.1]);
				0.25.wait;
			});
		}
	});
};
)

t = f.value.play(quant: 4);		// start on next 4-beat boundary

u = f.value.play(quant: [4, 0.5]);	// next 4-beat boundary + a half-beat

t.stop; u.stop;
::

]
@section{section}
 Using data routines in note sequencing

The previous example controls the generation of one parameter (pitch) by looping over an array inside the Task. What if you want to control several parameters?

Remember that routines can also generate data, in addition to their scheduling capabilities. You can refer to as many data routines as you want in your sequence.


@racketblock[
(
var midi, dur;
midi = Routine({
	[60, 72, 71, 67, 69, 71, 72, 60, 69, 67].do({ |midi| midi.yield });
});
dur = Routine({
	[2, 2, 1, 0.5, 0.5, 1, 1, 2, 2, 3].do({ |dur| dur.yield });
});

SynthDef(\smooth, { |freq = 440, sustain = 1, amp = 0.5|
	var sig;
	sig = SinOsc.ar(freq, 0, amp) * EnvGen.kr(Env.linen(0.05, sustain, 0.1), doneAction: Done.freeSelf);
	Out.ar(0, sig ! 2)
}).add;

r = Task({
	var delta;
	while {
		delta = dur.next;
		delta.notNil
	} {
		Synth(\smooth, [freq: midi.next.midicps, sustain: delta]);
		delta.yield;
	}
}).play(quant: TempoClock.default.beats + 1.0);
)
::

Note that routines are used for the data, but task is used for play. Also, unlike the previous infinite sequences, this one stops when it runs out of data. That's the purpose of the while loop -- it continues only as long as the 'dur' data stream keeps pumping out values. (See the link::Reference/Control-Structures:: helpfile for more on strong::while::.)

There must be an easier way to write the data streams -- repeatedly writing the same do loop is certainly inconvenient. In fact, there is such a way, covered in the next tutorial: sequencing with patterns.

(Here we use quant simply to delay Task onset by one beat. This is because it takes some time for the synthdef to be ready for use on the server. Without it, the first note would not be heard.)

]
@section{section}
 A note on server messaging and timing

Using Synth as in the preceding examples can result in small but sometimes noticeable timing inaccuracies. This is because it takes a short time to transmit OSC messages from your code to the server, and this time is not always constant. SuperCollider deals with this by giving you the option to send the message with a timestamp telling the server exactly when the message should take effect. A strong::latency:: value is used to calculate the timestamp.

Latency works by adding itself to the current time on the clock. If all the messages go out with the same latency value, their timing will be precise relative to each other and to the clock. The link::Guides/ServerTiming:: help file explains in more detail how this works, but you don't really need to know all of that in order to use it. The main point is to use a consistent, small latency value for perfect timing. (A Server object has a latency variable that you can use for consistency.)

Here's an example illustrating the kinds of inaccuracy you might hear. The inaccuracy may be more or less noticeable on different systems. It uses the \singrain SynthDef above and plays 10 notes per second.


@racketblock[
(
t = Task({
	loop {
		Synth(\singrain, [freq: exprand(400, 1200), sustain: 0.08]);
		0.1.wait;
	}
}).play;
)

t.stop;
::

The easiest way to add latency to your outgoing Synths is with the Server strong::makeBundle:: method. Don't worry about how it works for now -- the important thing is that it uses the first value for latency, and runs the messages produced by the function according to that latency.

]

@racketblock[
(
t = Task({
	loop {
		s.makeBundle(s.latency, {
			Synth(\singrain, [freq: exprand(400, 1200), sustain: 0.08]);
		});
		0.1.wait;
	}
}).play;
)

t.stop;
::

See also:

link::Classes/Routine::, link::Classes/Task::, link::Classes/Quant::, link::Guides/ServerTiming::, link::Guides/Bundled-Messages::

]
@section{section}
 Suggested Exercise

Make a more interesting SynthDef to replace the \smooth SynthDef. Use more arguments for greater variability. Then change the data streams in the 'Over the Rainbow' example and add new data streams to play a different tune, more expressively.

____________________

This document is part of the tutorial strong::Getting Started With SuperCollider::.

Click here to go on to the next section: link::Tutorials/Getting-Started/16-Sequencing-with-Patterns::

Click here to return to the table of Contents: link::Tutorials/Getting-Started/00-Getting-Started-With-SC::


