#lang scribble/manual
@(require (for-label racket))

@title{InterplPairs}
 envelope specification@section{related}
  Classes/InterplEnv
@section{categories}
  Control, Envelopes

@section{description}

Takes an array of [x, y] pairs and a curve value for all break points.
x values can be negative (for use in indexing with negative values or signals).
See link::Classes/InterplEnv:: Help for more info.

@section{Examples}
 


@racketblock[
a = InterplPairs([[0, 1], [1, 2], [2, 0]], \sin);
a.plot;

a = InterplPairs([[-1, 1], [0, 2], [1, 0]], \sin);
a.plot;
a.at(-0.5);
a.at(0.2);
]


