#lang scribble/manual
@(require (for-label racket))

@title{BAllPass}
 All Pass Filter@section{categories}
  UGens>Filters>BEQSuite
@section{related}
  Classes/SOS, Classes/BLowPass, Classes/BLowPass4, Classes/BHiPass, Classes/BHiPass4, Classes/BPeakEQ, Classes/BLowShelf, Classes/BHiShelf, Classes/BBandPass, Classes/BBandStop

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
  mul
@section{argument}
  add

@section{examples}
 

@racketblock[
s.boot;
(
z = { // thru
BAllPass.ar(
	SoundIn.ar([0, 1]),
	MouseX.kr(10, 18000, \exponential),
	0.8, // rq
	0.5); // mul
}.play)
z.release;

(
z = { // like a bandpass
	var sig;
	sig = SoundIn.ar([0,1]), * 0.5;
	BAllPass.ar(sig, MouseX.kr(10, 18000, \exponential), 0.8) + sig.neg
}.play)
z.release;
::
]


