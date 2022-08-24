#lang scribble/manual
@(require (for-label racket))

@title{BLowPass4}
 24db/oct rolloff - 4th order resonant Low Pass Filter [1]@section{categories}
  UGens>Filters>BEQSuite
@section{related}
  Classes/SOS, Classes/BLowPass, Classes/BHiPass, Classes/BHiPass4, Classes/BPeakEQ, Classes/BLowShelf, Classes/BHiShelf, Classes/BBandPass, Classes/BBandStop, Classes/BAllPass

@section{description}

The B equalization suite is based on the Second Order Section (link::Classes/SOS::) biquad UGen.

@section{note}
 
Biquad coefficient calculations imply certain amount of CPU overhead. These plugin UGens contain optimizations such that the coefficients get updated only when there has been a change to one of the filter's parameters. This can cause spikes in CPU performance and should be considered when using several of these units.
::

[1] this is a composite pseudo link::Classes/UGen::. BLowPass4 is built by cascading 2 link::Classes/SOS:: sections.

@section{classmethods}
 

@section{method}
  ar
@section{argument}
  in
input signal to be processed.
@section{argument}
  freq
cutoff frequency.
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
z = {
	BLowPass4.ar(
		SoundIn.ar([0,1]),
		MouseX.kr(10, 20000, \exponential), // cutoff freq.
		MouseY.kr(0.1, 1.0, \linear), // rq - compensate for 2 biquad sections.
		0.5); // mul
}.play)
z.release;
::
]


