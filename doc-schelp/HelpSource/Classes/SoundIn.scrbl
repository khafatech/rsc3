#lang scribble/manual
@(require (for-label racket))

@title{SoundIn}
 Read audio from hardware inputs@section{categories}
  UGens>InOut
@section{related}
  Classes/In, Classes/ServerOptions

@section{description}

SoundIn is a convenience UGen to read audio from the input of your computer or soundcard. It is a wrapper link::Classes/UGen:: based on link::Classes/In::, which offsets the index such that 0 will always correspond to the first input regardless of the number of inputs present.

@section{note}
 
On Intel based Macs, reading the built-in microphone or input may require creating an aggregate device in AudioMIDI Setup.


@racketblock["open -a 'Audio MIDI Setup'".unixCmd; // execute this to launch it::
::

]
@section{classmethods}
 
@section{method}
  ar

@section{argument}
  bus
the channel (or array of channels) to read in. These start at 0, which will correspond to the first audio input.

@section{argument}
  mul

@section{argument}
  add

@section{examples}
 

@racketblock[
// world's most expensive patchcord (use headphones to avoid feedback)
{ SoundIn.ar(0) }.play;

// stereo version
{ SoundIn.ar([0, 1]) }.play;

// scope input; silent output
{ Amplitude.kr(SoundIn.ar(0)); }.scope;
::
]


