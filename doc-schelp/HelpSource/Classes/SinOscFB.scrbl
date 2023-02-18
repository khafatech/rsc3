#lang scribble/manual
@(require (for-label racket))

@title{SinOscFB}
 Feedback FM oscillator@section{related}
  Classes/SinOsc, Classes/FSinOsc, Classes/PMOsc
@section{categories}
   UGens>Generators>Deterministic, UGens>Generators>Chaotic


@section{description}


SinOscFB is a sine oscillator that has phase modulation feedback; its output plugs back into the phase input.
Basically this allows a modulation between a sine wave and a sawtooth like wave. Overmodulation causes chaotic oscillation. It may be useful if you want to simulate feedback FM synths.




@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 freq

The base frequency of the sine oscillator in Hertz.

@section{argument}
 feedback

The second argument is the amplitude of phase feedback in radians.

@section{argument}
 mul

Output will be multiplied by this value.

@section{argument}
 add

This value will be added to the output after any multiplication.


@section{Examples}
 


@racketblock[


{SinOscFB.ar(440,MouseX.kr(0,4))*0.1}.play


{SinOscFB.ar(MouseY.kr(10,1000,'exponential'),MouseX.kr(0.5pi,pi))*0.1}.play


{SinOscFB.ar(100*SinOscFB.ar(MouseY.kr(1,1000,'exponential'))+200,MouseX.kr(0.5pi,pi))*0.1}.play


// Scope the wave form
{ SinOscFB.ar([400,301], MouseX.kr(0,4),0.3); }.scope;

::
]


