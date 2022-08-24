#lang scribble/manual
@(require (for-label racket))

@title{Pgate}
 gated stream advances only when an event key is true@section{related}
  Classes/Pn
@section{categories}
  Streams-Patterns-Events>Patterns>Repetition

@section{description}


Pgate advances its subpattern whenever strong::key:: is true. Pgate must be used within an Event pattern.

@section{Examples}
 


@racketblock[
// Pn advances Pgate each time its subpattern is repeated
(
Pbind(

	\degree,	Pn(Pseq((0..7)), inf, \step),
	\mtranspose,	Pgate(Pwhite(0,5), inf, \step),
	\dur, 0.2
).play
)


// Two different Pgates advanced at two different rates
(
Pbind(

	\scale,		Scale.minor,

	\foo,		Pn(Pseq((0..2)),inf,  \step1),
	\degree,	Pn(Pseq((0..7).mirror), inf, \step),
	\ctranspose,	Pgate(Pwhite(0,5), inf, \step) +
				Pgate(Pseq([0,7,0,-7], inf), inf, \step1),
	\dur, 0.2
).play
)
::
]


