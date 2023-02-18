#lang scribble/manual
@(require (for-label racket))

@title{Hilbert}
 Applies the Hilbert transform to an input signal.@section{related}
  Classes/HilbertFIR, Classes/FreqShift
@section{categories}
   UGens>Filters>Nonlinear


@section{description}


Returns two channels with the original signal and a copy of that signal that has been shifted in phase by 90 degrees (0.5 pi radians). Hilbert outputs two channels containing the input signal and the transformed signal. Due to the method used, distortion occurs in the upper octave of the frequency spectrum (See HilbertFIR for an FFT implementation that avoids this, but introduces a significant delay).


@section{classmethods}
 

@section{method}
 ar

@section{argument}
 in

The input signal to transform.


@section{argument}
 mul

Output will be multiplied by this value.


@section{argument}
 add

This value will be added to the output.


@section{Examples}
 


@racketblock[
s.boot;
s.scope;
a = {Hilbert.ar(SinOsc.ar(100)) * -20.dbamp}.play;
a.release;
::
]


