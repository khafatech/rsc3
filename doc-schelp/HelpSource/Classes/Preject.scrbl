#lang scribble/manual
@(require (for-label racket))

@title{Preject}
 Reject values from a pattern@section{categories}
  Streams-Patterns-Events>Patterns>Filter
@section{related}
  Classes/Pselect, Classes/Pcollect

@section{description}

Rejects values for which the function returns true. The value is passed to the function.


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
a = Preject({ arg item; item == 1 }, Pseq(#[1, 2, 3],inf));
x = a.asStream;
9.do({ x.next.postln; });
)
::

The message reject returns a Preject when passed to a pattern
]

@racketblock[
(
var a, b;
a = Pseq(#[1, 2, 3],inf).reject({ arg item; item == 1 });
a.postln;
x = a.asStream;
9.do({ x.next.postln; });
)
::
]


