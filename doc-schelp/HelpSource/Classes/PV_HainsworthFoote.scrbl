#lang scribble/manual
@(require (for-label racket))

@title{PV_HainsworthFoote}
 FFT onset detector.@section{related}
  Classes/PV_JensenAndersen
@section{categories}
   UGens>FFT


@section{description}


FFT onset detector based on work described in emphasis:: Hainsworth, S. (2003) Techniques for the Automated Analysis of Musical Audio. PhD, University of Cambridge engineering dept. ::
See especially p128. The Hainsworth metric is a modification of the Kullback Liebler
distance.


The onset detector has general ability to spot spectral change, so may
have some ability to track chord changes  aside from obvious transient
jolts, but there's no guarantee it won't be confused by frequency
modulation artifacts.


Hainsworth metric on it's own gives good results but Foote might be
useful in some situations: experimental.


@section{classmethods}
 
@section{private}
  categories

@section{method}
 ar

@section{argument}
 buffer

FFT buffer.


@section{argument}
 proph

What strength of detection signal from Hainsworth metric to use.


@section{argument}
 propf

What strength of detection signal from Foote metric to use. The
Foote metric is normalised to (0..1).


@section{argument}
 threshold

Threshold hold level for allowing a detection.


@section{argument}
 waittime

If triggered, minimum wait until a further frame can cause
another spot (useful to stop multiple detects on heavy signals).


@section{Examples}
 


@racketblock[

//just Hainsworth metric with low threshold
(
SynthDef(\fftod, {
	var source1, detect;
	source1= AudioIn.ar(1);
	detect= PV_HainsworthFoote.ar(FFT(LocalBuf(2048),source1), 1.0, 0.0);
	Out.ar(0,SinOsc.ar([440,445],0,Decay.ar(0.1*detect,0.1)));
}).play(s);
)


//spot note transitions
(
SynthDef(\fftod, {
	var source1, detect;
	source1= LFSaw.ar(LFNoise0.kr(1,90,400),0,0.5);
	detect= PV_HainsworthFoote.ar(FFT(LocalBuf(2048),source1), 1.0, 0.0, 0.9, 0.5);
	Out.ar(0,Pan2.ar(source1,-1.0)+ Pan2.ar(SinOsc.ar(440,0,Decay.ar(0.1*detect,0.1)),1.0));
}).play(s);
)



//Foote solo- never triggers with threshold over 1.0, threshold under mouse control
(
SynthDef(\fftod, {
	var source1, detect;
	source1= AudioIn.ar(1);
	detect= PV_HainsworthFoote.ar(FFT(LocalBuf(2048),source1), 0.0, 1.0, MouseX.kr(0.0,1.1), 0.02);
	Out.ar(0,Pan2.ar(source1,-1.0)+ Pan2.ar(SinOsc.ar(440,0,Decay.ar(0.1*detect,0.1)),1.0));
}).play(s);
)


//compare to Amplitude UGen
(
SynthDef(\fftod, {
		var source1, detect;
		source1= AudioIn.ar(1);
		detect= (Amplitude.ar(source1)) > (MouseX.kr(0.0,1.1));
		Out.ar(0,Pan2.ar(source1,-1.0)+ Pan2.ar(SinOsc.ar(440,0,Decay.ar(0.1*detect,0.1)),1.0));
	}).play(s);
)

::

]


