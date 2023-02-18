#lang scribble/manual
@(require (for-label racket))

@title{PV_JensenAndersen}
 FFT feature detector for onset detection.@section{related}
  Classes/PV_HainsworthFoote
@section{categories}
   UGens>FFT


@section{description}


FFT feature detector for onset detection based on work described in
emphasis::
Jensen, K. & Andersen, T. H. (2003). Real-time Beat Estimation
Using Feature Extraction. In Proceedings of the Computer Music Modeling
and Retrieval Symposium, Lecture Notes in Computer Science. Springer
Verlag.
::

First order derivatives of the features are taken.

@racketblock[threshold::  may need to be set low to pick up on
changes.


]
@section{classmethods}
 
@section{private}
  categories
@section{method}
 ar

@section{argument}
 buffer

FFT buffer.


@section{argument}
 propsc

Proportion of spectral centroid feature.


@section{argument}
 prophfe

Proportion of high frequency energy feature.


@section{argument}
 prophfc

Proportion of high frequency content feature.


@section{argument}
 propsf

Proportion of spectral flux feature.


@section{argument}
 threshold

Threshold level for allowing a detection.


@section{argument}
 waittime

If triggered, minimum wait until a further frame can cause
another spot (useful to stop multiple detects on heavy signals).


@section{Examples}
 


@racketblock[
(
SynthDef(\fftod, { var source1, detect;
		source1 = AudioIn.ar(1);
		detect = PV_JensenAndersen.ar(FFT(LocalBuf(2048), source1),
			threshold:MouseX.kr(0.1,1.0));
		Out.ar(0, SinOsc.ar([440,445], 0, Decay.ar(0.1*detect, 0.1)));
	}).play(s);
)
::
]


