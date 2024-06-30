#lang scribble/manual
@(require (for-label racket))

@title{BBandStop}
 Band reject filter@section{categories}
  UGens>Filters>BEQSuite
@section{related}
  Classes/SOS, Classes/BLowPass, Classes/BLowPass4, Classes/BHiPass, Classes/BHiPass4, Classes/BPeakEQ, Classes/BLowShelf, Classes/BHiShelf, Classes/BBandPass, Classes/BAllPass

@section{description}

The B equalization suite is based on the Second Order Section (link::Classes/SOS::) biquad UGen.

@section{note}
 
Biquad coefficient calculations imply certain amount of CPU overhead. These plugin UGens contain optimizations such that the coefficients get updated only when there has been a change to one of the filter's parameters. This can cause spikes in CPU performance and should be considered when using several of these units.
::

@section{classmethods}
 

@section{method}
  ar
@section{argument}
  in
input signal to be processed.
@section{argument}
  freq
center frequency.
WARNING: due to the nature of its implementation frequency values close to 0 may cause glitches and/or extremely loud audio artifacts!
@section{argument}
  bw
the bandwidth in octaves between -3 dB frequencies.
@section{argument}
  mul
@section{argument}
  add

@section{examples}
 

@racketblock[
s.boot;
(
z = {
BBandStop.ar(
	SoundIn.ar([0,1]),
	MouseX.kr(20, 20000, \exponential),
	MouseY.kr(0.0, 10.0, \linear), // bw
	0.5); // mul
}.play)
z.release;

(
z = {
BBandStop.ar(
	WhiteNoise.ar ! 2,
	MouseX.kr(20, 20000, \exponential),
	MouseY.kr(0.0, 10.0, \linear), // bw
	0.1); // mul
}.play)
z.release;

(
z = { // drill...
BBandStop.ar(
	SinOsc.ar(1000) ! 2,
	MouseX.kr(800, 1200, \exponential),
	MouseY.kr(0.0, 10.0, \linear), // bw
	0.1); // mul
}.play)
z.release;

(
z = { // nada
BBandStop.ar(
	SinOsc.ar(1000) ! 2,
	1000,
	MouseY.kr(0.0, 10.0, \linear), // bw
	0.5); // mul
}.play)
z.release;
::
]

