#lang scribble/manual
@(require (for-label racket))

@title{ControlRate}
 Server control rate.@section{related}
  Classes/RadiansPerSample, Classes/SampleDur, Classes/SampleRate, Classes/SubsampleOffset
@section{categories}
   UGens>Info


@section{description}


Get the current control rate of the server.


@section{classmethods}
 

@section{method}
 ir

@section{returns}
 
The current control rate of the server.

equivalent to 1 / link::Classes/ControlDur::

@section{Examples}
 

@racketblock[
{ ControlRate.ir.poll }.play;
::

play a sine tone at control rate
]

@racketblock[
{ SinOsc.ar(ControlRate.ir) * 0.1 }.play;
::

]


