#lang scribble/manual
@(require (for-label racket))

@title{Pselect}
 Select values from a pattern@section{categories}
  Streams-Patterns-Events>Patterns>Filter
@section{related}
  Classes/Pcollect, Classes/Preject

@section{description}

Returns values for which the function returns true. The value is passed to the function.


@section{classmethods}
 

@section{method}
  new
@section{argument}
  func
A link::Classes/Function::. Receives values from 
@racketblock[pattern::.
]
@section{argument}
  pattern
A link::Classes/Pattern::.


@section{examples}
 

@racketblock[
(
var a, b;
a = Pselect({ arg item; item != 2 }, Pseq(#[1, 2, 3],inf));
x = a.asStream;
9.do({ x.next.postln; });
)
::

The message ]

@racketblock[select:: returns a Pselect when passed to a pattern.
]

@racketblock[
(
var a, b;
a = Pseq(#[1, 2, 3],inf).select({ arg item; item != 2 });
a.postln;
x = a.asStream;
9.do({ x.next.postln; });
)
::
]


