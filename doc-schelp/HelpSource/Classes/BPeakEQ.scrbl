#lang scribble/manual
@(require (for-label racket))

@title{BPeakEQ}
 Parametric equalizer@section{categories}
  UGens>Filters>BEQSuite
@section{related}
  Classes/SOS, Classes/BLowPass, Classes/BLowPass4, Classes/BHiPass, Classes/BHiPass4, Classes/BPeakEQ, Classes/BLowShelf, Classes/BHiShelf, Classes/BBandPass, Classes/BBandStop, Classes/BAllPass

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
  rq
the reciprocal of Q. bandwidth / cutoffFreq.
@section{argument}
  db
boost/cut the center frequency (in dBs).
@section{argument}
  mul
@section{argument}
  add

@section{examples}
 

@racketblock[
s.boot;
(
z = { // toy with boost/cut
BPeakEQ.ar(
	SoundIn.ar([0,1]),
	MouseX.kr(20, 20000, \exponential),
	0.8, // rq
	MouseY.kr(12.0, -12.0, \linear),
	0.5); // mul
}.play)
z.release;

(
z = { // toy with rq
BPeakEQ.ar(
	SoundIn.ar([0,1]),
	MouseX.kr(20, 20000, \exponential),
	MouseY.kr(10, 0.4, \linear), // rq
	6, // boost/cut - +6dB
	0.5); // mul
}.play)
z.release;
::
]


