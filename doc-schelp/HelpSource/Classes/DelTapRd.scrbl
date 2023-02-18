#lang scribble/manual
@(require (for-label racket))

@title{DelTapRd}
@section{categories}
  UGens>Buffer, UGens>Delays
 Tap a delay line from a DelTapWr UGen@section{related}
  Classes/DelTapWr

@section{description}

Tap a delay line from a link::Classes/DelTapWr:: UGen.

@section{note}
  If you run a 
@racketblock[DelTapRd.ar:: and a ]

@racketblock[DelTapWr.ar:: in tandem, keep in mind that they read and write in blocks
equal to the server's block size. If the delay time is greater than the buffer size minus a block, the write and read
heads might interfere in unintended ways. Use a slightly larger buffer if this happens. ::

]
@section{classmethods}
 
@section{private}
  categories

@section{method}
  ar, kr

@section{argument}
  buffer
buffer where DelTapWr has written signal.  Max delay time is based on buffer size.
@section{argument}
  phase
the current phase of the DelTapWr UGen. This is the output of DelTapWr.
@section{argument}
  delTime
A delay time in seconds.
@section{argument}
  interp
the kind of interpolation to be used. 1 is none, 2 is linear, 4 is cubic.
@section{argument}
  mul
@section{argument}
  add

@section{examples}
 
See link::Classes/DelTapWr#@section{examples}
  for examples.



