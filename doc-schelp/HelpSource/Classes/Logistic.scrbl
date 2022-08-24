#lang scribble/manual
@(require (for-label racket))

@title{Logistic}
 Chaotic noise function@section{related}
  Classes/Crackle, Classes/LatoocarfianN
@section{categories}
   UGens>Generators>Chaotic


@section{description}


A noise generator based on the logistic map:

@racketblock[
y = chaosParam * y * (1.0 - y)
::

]
@section{classmethods}
 
@section{private}
  categories

@section{method}
 ar, kr

@section{argument}
 chaosParam
a parameter of the chaotic function with useful values from 0.0 to 4.0. Chaos occurs from 3.57 up. Don't use values outside this range if you don't want the UGen to blow up.

@section{argument}
 freq
Frequency of calculation; if over the sampling rate, this is clamped to the sampling rate

@section{argument}
  init
Initial value of y in the equation above

@section{argument}
 mul
Output will be multiplied by this value.

@section{argument}
 add
This value will be added to the output.

@section{discussion}
 
y will stay in the range of 0.0 to 1.0 for normal values of the chaosParam. This leads to a DC offset, and may cause a pop when you stop the Synth. For output you might want to combine this UGen with a LeakDC or rescale around 0.0 via mul and add: see example below.

@section{examples}
 

@racketblock[
// default values
{ Logistic.ar }.play

// onset of chaos
{ Logistic.ar(Line.kr(3.55, 3.6, 5), 1000) }.play

// explore via Mouse
{ Logistic.ar(MouseX.kr(3, 3.99), MouseY.kr(10, 10000, 'exponential'), 0.25, 0.5) }.play
::

]


