#lang scribble/manual
@(require (for-label racket))

@title{Pnsym}
 use a pattern of symbols to embed Pdefns@section{categories}
  Libraries>JITLib>Patterns, Live Coding
@section{related}
  Classes/Pdefn

@section{description}

for event patterns see link::Classes/Psym::. Overview: link::Overviews/JITLib::.

@section{ClassMethods}
 

@section{method}
 new

@section{argument}
 pattern
a pattern that returns symbols or characters. Arrays are converted to parallel patterns ( link::Classes/Ptuple:: ).

@section{argument}
 dict
the dictionary to be used for lookup. By default, this is 
@racketblock[Pdefn.all::, so one can embed Pdefns by name.

]
@section{InstanceMethods}
 

@section{method}
 dict
set the dictionary to be used.

@section{Examples}
 


@racketblock[
(
// load a synthdef
s.boot;
SynthDef("gpdef",
	{ arg out=0, freq=440, sustain=0.05, amp=0.1, pan;
		var env;
		env = EnvGen.kr(Env.perc(0.01, sustain), doneAction: Done.freeSelf) * amp;
		Out.ar(out, Pan2.ar(SinOsc.ar(freq, 0, env), pan))
	}).add;
)

Pdefn(\x, Pn(1, 3));
Pdefn(\y, Prand([5, 9, 1], 2));
Pdefn(\z, Pdefn(\y) * 2);

(
Pdef(\play,
	Pbind(
		\instrument, \gpdef,
		\harmonic, Pnsym(Pseq([\x, \x, Prand([\x, \y]), [\z, \y], \y], inf)).trace,
		\dur, 0.2, \note, 10
	)
).play;
)

// change root pattern:
Pdefn(\x, Pn(2, 3));
Pdefn(\x, Pseq([1, 3, 1, 2, 1, 4, 5]));
Pdefn(\x, Pseq([1, 3, 1, 2, [1, 3], 4, 5]));
::
]


