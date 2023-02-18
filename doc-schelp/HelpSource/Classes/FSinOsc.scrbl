#lang scribble/manual
@(require (for-label racket))

@title{FSinOsc}
 Fast sine oscillator.@section{related}
  Classes/SinOsc, Classes/SinOscFB
@section{categories}
   UGens>Generators>Deterministic

@section{description}


Very fast sine wave generator (2 PowerPC instructions per output sample!)
implemented using a ringing filter. This generates a much cleaner sine
wave than a table lookup oscillator and is a lot faster. However, the
amplitude of the wave will vary with frequency. Generally the amplitude
will go down as you raise the frequency and go up as you lower the frequency.

@section{warning}
 
In the current implementation, the amplitude can blow up if the
frequency is modulated by certain alternating signals.
::


@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 freq
Frequency in Hertz.

@section{argument}
 iphase
Initial phase offset.

@section{argument}
 mul
Output will be multiplied by this value.

@section{argument}
 add
This value will be added to the output.


@section{Examples}
 


@racketblock[
{ FSinOsc.ar(800) * 0.2 }.play;

{ FSinOsc.ar(XLine.kr(200, 4000, 1)) * 0.2 }.play;

// loses amplitude towards the end
{ FSinOsc.ar(FSinOsc.ar(XLine.kr(4, 401, 8), 0.0, 200, 800)) * 0.2 }.play;

::

]


