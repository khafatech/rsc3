#lang scribble/manual
@(require (for-label racket))

@title{SinOsc}
 Interpolating sine wavetable oscillator.@section{related}
  Classes/Osc, Classes/FSinOsc, Classes/SinOscFB, Classes/PMOsc, Classes/Klang
@section{categories}
   UGens>Generators>Deterministic


@section{description}


Generates a sine wave.
Uses a wavetable lookup oscillator with linear interpolation.
Frequency and phase modulation are provided for audio-rate modulation.
Technically, 
@racketblock[SinOsc:: uses the same implementation as  link::Classes/Osc::  except that its table is fixed to be a sine wave made of ]

@racketblock[8192:: samples.

]
@section{subsection}
  Other sinewaves oscillators

@section{LIST}
 
## link::Classes/FSinOsc:: – fast sinewave oscillator
## link::Classes/SinOscFB:: – sinewave with phase feedback
## link::Classes/PMOsc:: – phase modulation sine oscillator
## link::Classes/Klang:: – bank of sinewave oscillators
## link::Classes/DynKlang:: – modulable bank of sinewave oscillators
::


@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 freq
Frequency in Hertz.
Sampled at audio-rate.

@section{argument}
 phase
Phase in radians.
Sampled at audio-rate.
@section{note}
 phase values should be within the range +-8pi. If your phase values are larger then simply use 
@racketblock[.mod(2pi):: to wrap them.::

]
@section{argument}
 mul
Output will be multiplied by this value.

@section{argument}
 add
This value will be added to the output.


@section{Examples}
 


@racketblock[

// create an audio-rate sine wave at 200 Hz,
// starting with phase 0 and an amplitude of 0.5
{ SinOsc.ar(200, 0, 0.5) }.play;

// modulate the frequency with an exponential ramp
{ SinOsc.ar(XLine.kr(2000, 200), 0, 0.5) }.play;

// more complex frequency modulation
{ SinOsc.ar(SinOsc.ar(XLine.kr(1, 1000, 9), 0, 200, 800), 0, 0.25) }.play;

// phase modulation (see also PMOsc)
{ SinOsc.ar(800, SinOsc.ar(XLine.kr(1, 1000, 9), 0, 2pi), 0.25) }.play;

::

]


