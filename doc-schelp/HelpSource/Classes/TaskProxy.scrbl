#lang scribble/manual
@(require (for-label racket))

@title{TaskProxy}
 event stream reference@section{categories}
  Libraries>JITLib>Patterns, Live Coding
@section{related}
  Classes/Tdef

@section{description}

Keeps a reference to a task (time pattern) that can be replaced while playing. It plays on when the old stream ended and a new stream is set and schedules the changes to the beat.

@section{ClassMethods}
 

@section{method}
 new
create a new instance with a function (the source). the source should be a strong::routine function:: (see link::Classes/Tdef::) or a strong::pattern:: of time values.

@section{method}
 default
a default source, if none is given. the default is a loop that does nothing with a 1.0 beat wait time.

@section{method}
 defaultQuant
set the default quantization value for the class. (default: 1.0). can be a pair [quant, offset]

@section{InstanceMethods}
 

@section{method}
 source
set the source. If a quantization is given, schedule this change to the next beat the object is a strong::routine function::, which is evaluated in a protected way, so that failure will notify the proxy that it has stopped. The object can also be a strong::pattern:: of time values.

@section{method}
 clear
set the source to nil

@section{method}
 quant
get or set the quantization value. can be a pair [quant, offset]

@section{method}
 condition
provide a condition under which the pattern is switched when a new one is inserted. the stream value and a count is passed into the function. the methods strong::count_(n):: simply counts up to n and switches the pattern then

@section{method}
 reset
switch the pattern immediately. (stuck conditions can be subverted by this)

@section{method}
 envir
provide a default environment for the proxy. If given, it is used as an environment for the routine function. When set for the first time, the routine pattern is rebuilt.

@section{method}
 set
set arguments in the environment. If there is none, it is created and the routine pattern is rebuilt.

@section{method}
 endless
returns a link::Classes/Prout:: that plays the proxy endlessly, replacing strong::nil:: with a strong::default:: value (1 s. wait time). This allows to create streams that idle on until a new pattern is inserted.

@section{subsection}
 a) using it as stream reference

@section{method}
 source
set the routine function / pattern (internally done by *new(key, obj)

@section{method}
 embedInStream
just like any stream, embeds itself in stream

@section{subsection}
 b) using it as EventStreamPlayer

@section{method}
 play
starts the TaskProxy and creates a player. if you want to play multiple instances, use strong::.playOnce(clock, protoEvent, quant)::

@section{argument}
 argClock
which clock to use. if nil then the TempoClock.default is used.

@section{argument}
 doReset
A link::Classes/Boolean::

@section{argument}
 quant
can be an array of [quant, phase]

@section{method}
 stop
stops the player

@section{method}
 player
the current player (if the TaskProxy is simply used in other streams this is nil)

@section{method}
 pause, resume, reset
perform player method

@section{method}
 isPlaying
returns true if TaskProxy is running. if a TaskProxy is playing and its stream ends, it will schedule a stream for playing as soon as a new one is assigned to it.

@section{Examples}
 

@section{subsection}
 a) using TaskProxy as a player


@racketblock[
// create an empty Tdef and play it.
x = TaskProxy.new;
x.play;


x.source = { loop { "ggggggggggggggggg9999ggg999ggg999gg".scramble.postln; 0.5.wait; } };


x.source = { loop { "---------////----------------------".scramble.postln; 0.25.wait; } };
x.source = { loop { thisThread.seconds.postln; 1.wait; } };
x.source = { loop { thisThread.seconds.postln; 1.01.wait; } };

TempoClock.default.tempo = 2;

x.source = { "the end".postln };
x.source = { "one more".postln };
x.source = { 10.do { "ten more".scramble.postln; 0.25.wait; } };
x.source = { loop { "many more".scramble.postln; 0.25.wait; } };

TempoClock.default.tempo = 1;

x.stop;
x.play;
x.stop;
::


]

@racketblock[
// sound example

(
// load a synthdef
s.boot;
SynthDef("pdef_grainlet",
	{ arg out=0, freq=440, sustain=0.05;
		var env;
		env = EnvGen.kr(Env.perc(0.01, sustain, 0.3), doneAction: Done.freeSelf);
		Out.ar(out, SinOsc.ar(freq, 0, env))
	}).add;
)
x.play;

(
x.source = {
	loop {
		s.sendMsg("/s_new", "pdef_grainlet", -1,0,0, \freq, rrand(600, 640));
		0.1.wait;
	}
}
)

(
x.source = {
	var x;
	x = Pseries(300, 20, 100).loop.asStream;
	loop {
		s.sendMsg("/s_new", "pdef_grainlet", -1,0,0, \freq, x.next);
		0.05.wait;
	}
}
)

(
x.source = {
	var x;
	x = Plazy { Pseries(300 + 300.rand, 10 + 30.rand, 10 + 30.rand) }.loop.asStream;
	loop {
		s.sendMsg("/s_new", "pdef_grainlet", -1,0,0, \freq, x.next);
		0.05.wait;
	}
}
)

// metronome
(
y = TaskProxy {
	loop { s.sendMsg("/s_new", "pdef_grainlet", -1,0,0, \freq, 1500); 1.wait; }
};
y.play;
)

// play ending stream once
(
x.source = {
	var x, dt;
	dt = [0.1, 0.125, 0.05].choose;
	x = Plazy { Pseries(1300 + 300.rand, 110 + 130.rand, 16) }.asStream;
	x.do { arg item;
		s.sendMsg("/s_new", "pdef_grainlet", -1,0,0, \freq, item);
		dt.wait;
	}
}
)

... and so on ...

x.stop;
y.stop;
::

]
@section{subsection}
 b) embedding TaskProxy into other Tasks / Routines


@racketblock[
(
#a, c = { TaskProxy.new } ! 2;
a.source = { "one".postln; 1.wait; "two".postln };
c.source = { var z; z = Synth(\default); 0.5.wait; z.release };
r = Task {
	"counting...".postln;
	2.wait;
	a.embedInStream;
	1.wait;
	c.embedInStream;
	"done.".postln;
};
)

r.play; // play a stream

c.source = { var z; z = Synth(\default, [\freq, 300]); 1.5.wait; z.release }; // change the def

r.reset;
r.play;

// of course TaskProxies can be used in other Tdefs:
(
b = TaskProxy.new;
b.source = {
	"counting...".postln;
	2.wait;
	a.embedInStream;
	1.wait;
	c.embedInStream;
	"done.".postln;
};
)
b.playOnce;

// if one wants to branch off a stream in a separate thread, asStream is used.
(
Routine {
	c.asStream.play;
	0.1.wait;
	c.asStream.play;
	0.1.wait;
	a.asStream.play;

}.play;
)
::
]


