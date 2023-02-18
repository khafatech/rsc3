#lang scribble/manual
@(require (for-label racket))

@title{Expression Sequence}
 sequence of expressions@section{categories}
  Language

A sequence of expressions separated by semicolons and optionally terminated by a semicolon are a single expression whose value is the value of the last expression. Such a sequence may be used anywhere that a normal expression may be used.

@racketblock[
max( b = a * 2; b + 5,  10);	// computes the maximum of b+5 and 10
::
In the above example, the sequence: ]

@racketblock[ b = a * 2; b + 5 :: acts as a single expression for the first argument to ]

@racketblock[max()::.

]


