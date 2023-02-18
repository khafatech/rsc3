#lang scribble/manual
@(require (for-label racket))

@title{InterplXYC}
 envelope specification@section{related}
  Classes/InterplEnv
@section{categories}
  Control, Envelopes

@section{description}

Takes sets of x, y and curve values and returns a new instance of InterplEnv.
x values can be negative (for use in indexing with negative values or signals).
See link::Classes/InterplEnv:: Help for more info.

@section{Examples}
 


@racketblock[
a = InterplXYC([0, 0, \lin], [1, 2, \sin], [2, 0]);
a.plot;

a = InterplXYC([[-1, 1, \sin], [0, 2, \lin], [1, 0]]);
a.plot;
a.at(-0.5);
a.at(0.2);
]


