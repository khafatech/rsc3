#lang scribble/manual
@(require (for-label racket))

@title{PdegreeToKey}
 index into a scale@section{related}
  Classes/Scale
@section{categories}
  Streams-Patterns-Events>Patterns>Math

@section{description}


Returns a series of notes derived from an index into a scale.

@section{ClassMethods}
 

@section{method}
 new

@section{argument}
 pattern
integer index into the scale.

@section{argument}
 scale
an array or pattern. If a pattern, it streams the scales accordingly.

@section{argument}
 stepsPerOctave
the number of steps per octave in the scale.

@section{Examples}
 


@racketblock[
(
Pbind(\note, PdegreeToKey(
			Pseq([1, 2, 3, 2, 5, 4, 3, 4, 2, 1], 2),
			#[0, 2, 3, 6, 7, 9],
			12
		),
	\dur, 0.25
).play;
)


(
var scales;
scales = #[[0, 2, 3, 6, 7, 9], [0, 1, 5, 6, 7, 9, 11], [0, 2, 3]];
Pbind(\note, PdegreeToKey(
			Pseq([1, 2, 3, 2, 5, 4, 3, 4, 2, 1], 4),
			Pstutter(3, Prand(scales, inf)),
			12
		),
	\dur, 0.25
).play;
)
::
]


