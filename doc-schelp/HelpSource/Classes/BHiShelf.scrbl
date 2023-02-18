#lang scribble/manual
@(require (for-label racket))

@title{BHiShelf}
 Hi Shelf@section{categories}
  UGens>Filters>BEQSuite
@section{related}
  Classes/SOS, Classes/BLowPass, Classes/BLowPass4, Classes/BHiPass, Classes/BHiPass4, Classes/BPeakEQ, Classes/BLowShelf, Classes/BBandPass, Classes/BBandStop, Classes/BAllPass

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
WARNING: due to the nature of its implementation frequency values close to 0 may cause glitches and/or extremely loud audio artifacts!
center frequency.
@section{argument}
  rs
the reciprocal of S. Shell boost/cut slope. When S = 1, the shelf slope is as steep as it can be and remain monotonically increasing or decreasing gain with frequency. The shelf slope, in dB/octave, remains proportional to S for all other values for a fixed 
@racketblock[freq/SampleRate.ir:: and ]

@racketblock[db::.
]
@section{argument}
  db
gain. boost/cut the center frequency in dBs.
@section{argument}
  mul
@section{argument}
  add

@section{examples}
 

@racketblock[
s.boot;
(
z = { // toy around with boost/cut
BHiShelf.ar(
	SoundIn.ar([0,1]),
	MouseX.kr(2200, 18000, \exponential),
	1.0, // rs
	MouseY.kr(18.0, -18.0, \linear),
	0.5); // mul
}.play)
z.release;

(
z = { // toy around with rs
BHiShelf.ar(
	SoundIn.ar([0,1]),
	MouseX.kr(2200, 18000, \exponential),
	MouseY.kr(0.1, 1.0, \linear), // rs
	6, // db
	0.5); // mul
}.play)
z.release;
::
]


