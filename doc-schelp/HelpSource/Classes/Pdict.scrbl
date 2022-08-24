#lang scribble/manual
@(require (for-label racket))

@title{Pdict}
 pattern that embeds patterns from a dictionary@section{categories}
  Libraries>JITLib>Patterns, Live Coding
@section{related}
  Classes/Pbind

@section{description}

A general purpose lookup stream.

@section{Examples}
 


@racketblock[
SynthDescLib.read;

(
e = (
	a: Pbind(\dur, 0.1, \degree, Pseq([0, 5, 4, 3, 2])),
	b: Pbind(\dur, 0.06, \degree, Pseq([7, 8, 7, 8])),
	c: Pbind(\dur, 0.3, \degree, Pseq([0, 1, 2], 2))
);

x = Pdict(e, Pseq([
			\a, \b,
			Prand([\a, \c])
		], 4)
	);
x.play;
)
::
]


