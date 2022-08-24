#lang scribble/manual
@(require (for-label racket))

@title{HilbertFIR}
 Applies the Hilbert transform to an input signal.@section{related}
  Classes/Hilbert, Classes/FreqShift
@section{categories}
   UGens>Filters>Nonlinear


@section{description}


Returns two channels with the original signal and a copy of that signal that has been shifted in phase by 90 degrees (0.5 pi radians). HilbertFIR outputs two channels containing the input signal and the transformed signal. HilbertFIR uses FFTs and a 90 degree phase shift to transform the signal, and results in a delay equal to the size of the buffer used for the FFT divided by the sample rate. The Hilbert UGen has less delay, but distorts in the upper octave of the frequency spectrum.


@section{classmethods}
 

@section{method}
 ar

@section{argument}
 in

The input signal to transform.


@section{argument}
 buffer

A Buffer to be used for the inter FFT processing. Best results with a size of 1024 or 2048. 1024 gives an acceptable result (though a little choppy), 2048 is much smoother but creates more delay.



@racketblock[
s.boot;
s.scope;
a = { HilbertFIR.ar(SinOsc.ar(100) * -20.dbamp, LocalBuf(2048) }.play;
a.release;
::
]


