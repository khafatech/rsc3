#lang scribble/manual
@(require (for-label racket))

@title{Tap}
 Single tap into a delay line@section{related}
  Classes/MultiTap, Classes/PlayBuf
@section{categories}
   UGens>Buffer, UGens>Delays>Buffer

@section{description}


The Tap UGen allows a single tap at a delay into a buffer.

Tap uses the link::Classes/PlayBuf:: UGen internally

@section{classmethods}
 
@section{private}
  categories

@section{method}
 ar

@section{argument}
 bufnum
The index of the buffer to use

@section{argument}
 numChannels
Number of channels of the buffer

@section{argument}
 delaytime
Tap delay; cannot be modulated

@section{examples}
 

@racketblock[
// Create a buffer.
b=Buffer.alloc(s, s.sampleRate, 1); //enough space for one second of mono audio

// Write to the Buffer with BufWr, read using two Taps, one for each ear!
(
SynthDef(\helpTap, {|bufnum|
	var source, capture;

	source= SoundIn.ar(0); //use headphones to avoid feedback
	capture= BufWr.ar(source, bufnum, Phasor.ar(0,1, 0, BufFrames.ir(bufnum),1));

	Out.ar(0, Tap.ar(bufnum, 1, [0.1,0.9])); //multichannel expansion, so one tap each ear
}).add;
)

x=Synth(\helpTap,[\bufnum, b]);

x.free;
::

]


