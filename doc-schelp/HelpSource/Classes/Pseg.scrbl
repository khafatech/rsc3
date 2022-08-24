#lang scribble/manual
@(require (for-label racket))

@title{Pseg}
 timed embedding of values@section{related}
  Classes/Ptime, Classes/Pstep, Classes/Env
@section{categories}
  Streams-Patterns-Events>Patterns>Time

@section{description}


Pseg defines a function of time as a breakpoint envelope using the same parameters as link::Classes/Env::. These patterns can be used to describe tempo or dynamic variations independent of the rhythmic patterns that express them.

@section{ClassMethods}
 

@section{method}
 new

@section{argument}
 levels
A link::Classes/Pattern:: that returns the levels. The first level is the initial value of the envelope, all subsequent values are interpolated.

@section{argument}
 durs
A link::Classes/Pattern:: that returns segments durations in beats.

@section{argument}
 curves
a link::Classes/Symbol::, link::Classes/Float::, or an link::Classes/Array:: of those. Determines the shape of the segments.

@section{argument}
 repeats
a number.

The possible values are:
@section{table}
 
## 
@racketblock[\step:: || || flat segments.
## ]

@racketblock[\linear:: || ]

@racketblock[\lin:: || linear segments, the default.
## ]

@racketblock[\exponential:: || ]

@racketblock[\exp:: || natural exponential growth and decay. In this case, the levels must all be nonzero and the have the same sign.
## ]

@racketblock[\sine:: || ]

@racketblock[\sin:: || sinusoidal S shaped segments.
## ]

@racketblock[\welch:: || ]

@racketblock[\wel:: || sinusoidal segments shaped like the sides of a Welch window.
## ]

@racketblock[\squared::  || ]

@racketblock[\sqr:: || squared segment.
## ]

@racketblock[\cubed:: || ]

@racketblock[\cub:: || cubed segment.
## a link::Classes/Float:: || || a curvature value for all segments. 0 means linear, positive and negative numbers curve the segment up and down.
## an link::Classes/Array:: of symbols or floats || || curvature values for each segment.
::

]
@section{Examples}
 


@racketblock[
// change a parameter
(
Pbind(
	\note,  Pseg( Pseq([1, 5],inf), Pseq([4, 1],inf), \linear),
	\dur, 0.1
).play;
)

(
Pbind(
	\freq,  Pseg( Pseq([400, 1500], inf), Pseq([4, 4],inf), Pseq([\linear, \exp],inf)),
	\dur, 0.1
).play;
)
::
]


