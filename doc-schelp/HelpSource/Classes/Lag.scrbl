#lang scribble/manual
@(require (for-label racket))

@title{Lag}
 Exponential lag@section{related}
  Classes/Lag2, Classes/Lag3, Classes/VarLag, Classes/LagUD
@section{categories}
   UGens>Filters>Linear


@section{description}

This is essentially the same as  link::Classes/OnePole::  except that
instead of supplying the coefficient directly, it is calculated from a 60
dB lag time. This is the time required for the filter to converge to
within 0.01% of a value. This is useful for smoothing out control
signals.

For linear and other alternatives, see link::Classes/VarLag::.

@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 in

The input signal.


@section{argument}
 lagTime

60 dB lag time in seconds.


@section{argument}
 mul

Output will be multiplied by this value.


@section{argument}
 add

This value will be added to the output.


@section{Examples}
 


@racketblock[
(
// used to lag pitch
{
    SinOsc.ar(                              // sine wave
        Lag.kr(                             // lag the modulator
            LFPulse.kr(4, 0, 0.5, 50, 400), // frequency modulator
            Line.kr(0, 1, 15)               // modulate lag time
        ),
        0,                                  // phase
        0.3                                 // sine amplitude
    )
}.play
)
::

]


