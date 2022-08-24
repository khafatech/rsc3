#lang scribble/manual
@(require (for-label racket))

@title{MouseY}
 Cursor tracking UGen.@section{related}
  Classes/KeyState, Classes/MouseButton, Classes/MouseX
@section{categories}
   UGens>User interaction


@section{description}


Cursor tracking UGen.


@section{classmethods}
 

@section{method}
 kr

@section{argument}
 minval

Value corresponding to the bottom edge of the screen.


@section{argument}
 maxval

Value corresponding to the top edge of the screen.


@section{argument}
 warp

Mapping curve. 0 is linear, 1 is exponential (e. g. for freq or
times). Alternatively you can specify: 'linear' or 'exponential'.


@section{argument}
 lag

Lag factor to dezipper cursor movement.


@section{Examples}
 


@racketblock[

{ SinOsc.ar(MouseY.kr(40, 10000, 1), 0, 0.1) }.play;

::

]


