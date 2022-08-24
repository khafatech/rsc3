#lang scribble/manual
@(require (for-label racket))

@title{SpecFlatness}
 Spectral Flatness measure@section{categories}
  UGens>FFT
@section{related}
  Classes/SpecCentroid, Classes/SpecPcile

@section{description}

Given an link::Classes/FFT:: strong::chain:: this calculates the emphasis::Spectral Flatness:: measure, defined as a power spectrum's geometric mean divided by its arithmetic mean. This gives a measure which ranges from approx 0 for a pure sinusoid, to approx 1 for white noise.

The measure is calculated linearly. For some applications you may wish to convert the value to a decibel scale - an example of such conversion is shown below.

@section{classmethods}
 
@section{method}
  kr

@section{argument}
  buffer
an link::Classes/FFT:: chain.

@section{examples}
 


@racketblock[
s.boot;
b = Buffer.alloc(s,2048,1);

(
{ // Example - vary mixture of white noise and pure tone with the mouse
var in, chain, flat, flatdb, flatdbsquish;
in = XFade2.ar(WhiteNoise.ar, SinOsc.ar, MouseX.kr(-1,1));
chain = FFT(b, in);
Out.ar(0, in * 0.1);

flat = SpecFlatness.kr(chain);

flatdb = 10 * flat.log; // Convert to decibels
flatdbsquish = LinLin.kr(flatdb, -45, -1.6, 0, 1).max(-10); // Rescale db roughly to 0...1.

flat.poll(10, "flatness: ");
flatdb.poll(10, "flatness (db): ");

Out.kr(0, [flat, flatdbsquish]);
}.scope;
)

(
{ // Now try with your own voice
var in, chain;
in = SoundIn.ar([0,1]).mean;
chain = FFT(b, in);
Out.kr(0, [in, SpecFlatness.kr(chain).poll(1, "flatness: ")]);
}.scope;
)
::
]


