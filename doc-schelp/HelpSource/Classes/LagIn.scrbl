#lang scribble/manual
@(require (for-label racket))

@title{LagIn}
 Read a control signal from a bus with a lag@section{related}
  Classes/In, Classes/InFeedback
@section{categories}
   UGens>InOut


@section{description}


Read a control signal from a bus with a lag.


@section{classmethods}
 

@section{method}
 kr

@section{argument}
 bus

The index of the bus to read in from.


@section{argument}
 numChannels

The number of channels (i.e. adjacent buses) to read in. You
cannot modulate this number.


@section{argument}
 lag

Lag factor.




