#lang scribble/manual
@(require (for-label racket))

@title{SpecCentroid}
 Spectral centroid@section{categories}
  UGens>FFT
@section{related}
  Classes/SpecFlatness, Classes/SpecPcile

@section{description}

Given an link::Classes/FFT:: strong::chain::, this measures the emphasis::spectral:: centroid, which is the weighted mean frequency, or the "centre of mass" of the spectrum. (DC is ignored.)

This can be a useful indicator of the perceptual emphasis::brightness:: of a signal.

@section{classmethods}
 
@section{method}
  kr

@section{argument}
  buffer
an link::Classes/FFT:: chain.

@section{examples}
 

A link::Classes/Blip:: oscillator is ideal for demonstrating this because the number of harmonics is directly manipulated: as the number of harmonics increases, the centroid is pushed higher. In the example, left-to-right changes the number of harmonics, but up-to-down changes the fundamental pitch; note the different effects of these two on the centroid.


@racketblock[
s.boot;
b = Buffer.alloc(s,2048,1);
(
x = {
var in, chain, freq, rq, centroid;

//freq = LFPar.kr(0.3).exprange(100, 1000);
freq = MouseY.kr(1000, 100, 1);

in = Blip.ar(freq, MouseX.kr(1, 100, 1));

chain = FFT(b, in);

centroid = SpecCentroid.kr(chain);

Out.ar(0, in.dup * 0.1);
centroid.poll(10);
}.play(s);
)

x.free;
::
]

