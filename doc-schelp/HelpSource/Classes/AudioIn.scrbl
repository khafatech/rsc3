#lang scribble/manual
@(require (for-label racket))

@title{AudioIn}
 Read audio input.@section{related}
  Classes/In, Classes/SoundIn
@section{categories}
   UGens>InOut


@section{description}


Reads audio from the sound input hardware.

@section{note}
 
This is provided for backwards compatibility with SC2 code. For normal use link::Classes/SoundIn::, which has bus numbers beginning at 0, as AudioIn may be deprecated and removed at some point in the future.
::

@section{classmethods}
 

@section{method}
 ar

@section{argument}
 channel

Input channel number to read. Channel numbers begin at 1.

@section{argument}
 mul

@section{argument}
 add

@section{Examples}
 
Patching input to output

@racketblock[
// patching input to output

// beware of the feedback

(
ServerOptions.inDevices.postln;	//  post available audio input devices
s.meter;	// display level meters for monitoring
SynthDef("help-AudioIn", {
	var input = AudioIn.ar(1); // first input
	// delay output to tame feedback in case of microphones are configured:
	Out.ar(0, CombN.ar(input * -25.dbamp, 0.5, 0.5, 0.001))
}).play
)
::

]


