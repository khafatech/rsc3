#lang scribble/manual
@(require (for-label racket))

@title{NumChannels}
 Ensures the number of output channels@section{categories}
  UGens>Multichannel

@section{description}

Ensures the output has the stated number of channels, regardless of the number of input channels.

@section{classmethods}
 

@section{method}
  ar
@section{argument}
  input
the audio signal
@section{argument}
  numChannels
an integer
@section{argument}
  mixdown
true/false, whether you want to mixdown or just use the first channel

@section{discussion}
 
Mono input is copied.
Multi-channels clumped and if 
@racketblock[mixdown:: is true mixed down, else the first channel used.

]
@section{Examples}
 

@racketblock[
(
{
	NumChannels.ar(
		SinOsc.ar(100,0,0.2), // 1 becomes 2
		2)
}.play
)

(
{
	NumChannels.ar(
		SinOsc.ar([100,200,300],0,0.2), // 3 becomes 2
		2)
}.play
)

(
{
	NumChannels.ar(
		SinOsc.ar([100,200,300,100],0,0.2), // 4 becomes 2

		2)
}.play
)
::

]


