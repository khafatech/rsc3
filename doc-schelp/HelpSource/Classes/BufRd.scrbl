#lang scribble/manual
@(require (for-label racket))

@title{BufRd}
 Buffer reading oscillator.@section{related}
  Classes/BufWr
@section{categories}
   UGens>Buffer


@section{description}


Read the content of a buffer at an index.

In comparison to link::Classes/PlayBuf:: :
PlayBuf plays through the buffer by itself, BufRd only moves its read point by the phase input and
therefore has no pitch input. BufRd has variable interpolation.

@section{classmethods}
 
@section{private}
  categories
@section{method}
 ar, kr

@section{argument}
 numChannels
Number of channels that the buffer will be. This must be a fixed
integer. The architecture of the SynthDef cannot change after it
is compiled.
@section{note}
 
If you supply a  
@racketblock[bufnum::  of a buffer that has a
different  ]

@racketblock[numChannels::  then you have specified to
the BufRd, it will post a warning and output the channels it can.
::

]
@section{argument}
 bufnum
The index of the buffer to use.

@section{argument}
 phase
Audio rate modulateable index into the buffer.
@section{Warning}
  The phase argument only offers precision for addressing 2**24 samples (about 6.3 minutes at 44100Hz). ::

@section{argument}
 loop
1 means true, 0 means false. This is modulateable.


@section{argument}
 interpolation
1 means no interpolation, 2 is linear, 4 is cubic interpolation.

@section{instancemethods}
 
@section{private}
  init, argNamesInputsOffset, checkInputs

@section{Examples}
 


@racketblock[

(
// read a whole sound into memory
s = Server.local;
// note: not *that* columbia, the first one
b = Buffer.read(s, Platform.resourceDir +/+ "sounds/a11wlk01.wav");
)

//use any AUDIO rate ugen as an index generator

{ BufRd.ar(1, b, SinOsc.ar(0.1) * BufFrames.ir(b)) }.play;
{ BufRd.ar(1, b, LFNoise1.ar(1) * BufFrames.ir(b)) }.play;
{ BufRd.ar(1, b, LFNoise1.ar(10) * BufFrames.ir(b)) }.play;
{ BufRd.ar(1, b, LFTri.ar(0.1) + LFTri.ar(0.23) * BufFrames.ir(b)) }.play;
// original duration
{ BufRd.ar(1, b, LFSaw.ar(BufDur.ir(b).reciprocal).range(0, BufFrames.ir(b)) ) }.play;


//use a phasor index into the file

{ BufRd.ar(1, b, Phasor.ar(0, BufRateScale.kr(b), 0, BufFrames.kr(b))) }.play;


//change rate and interpolation
(
x = { arg rate=1, inter=2;
	BufRd.ar(1, b, Phasor.ar(0, BufRateScale.kr(b) * rate, 0, BufFrames.kr(b)), 1, inter)
}.play;
)

x.set(\rate, 0.9);
x.set(\rate, 0.6);
x.set(\inter, 1);
x.set(\inter, 0);


//write into the buffer with a BufWr
(
y = { arg rate=1;
	var in;
	in = SinOsc.ar(LFNoise1.kr(2, 300, 400), 0, 0.1);
	BufWr.ar(in, b, Phasor.ar(0, BufRateScale.kr(b) * rate, 0, BufFrames.kr(b)));
	0.0 //quiet
}.play;
)

//read it with a BufRd
(
x = { arg rate=1;
	BufRd.ar(1, b, Phasor.ar(0, BufRateScale.kr(b) * rate, 0, BufFrames.kr(b)))
}.play;
)



x.set(\rate, 5);
y.set(\rate, 2.0.rand);
x.set(\rate, 2);

b.free

::

]


