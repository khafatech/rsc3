#lang scribble/manual
@(require (for-label racket))

@title{ControlDur}
 Duration of one block@section{categories}
  UGens>Info
@section{related}
  Classes/ControlRate

@section{description}

Returns the current block duration of the server in seconds. Equivalent to  1 / link::Classes/ControlRate::.

@section{classmethods}
 
@section{method}
  ir

@section{examples}
 

@racketblock[
{ ControlDur.ir.poll }.play;

{ (1/ControlDur.ir).poll }.play;

{ ControlRate.ir.poll }.play;
::
]


