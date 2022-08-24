#lang scribble/manual
@(require (for-label racket))

@title{XOut}
 Send signal to a bus, crossfading with previous contents.@section{related}
  Classes/OffsetOut, Classes/Out, Classes/ReplaceOut
@section{categories}
   UGens>InOut


@section{description}


Send signal to a bus, crossfading with previous contents.

@racketblock[xfade::  is a level for the crossfade between what
is on the bus and what you are sending. The algorithm is equivalent to this:

]

@racketblock[
bus_signal = (input_signal * xfade) + (bus_signal * (1 - xfade));
::


See the link::Reference/Server-Architecture:: and link::Classes/Bus:: helpfiles for more information on
buses and how they are used.


]
@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 bus
The index of the bus to write out to. The lowest numbers are written to the audio hardware.

@section{argument}
 xfade
Crossfade level.

@section{argument}
 channelsArray
An Array of channels or single output to write out. You cannot change the size of this once a SynthDef has been built.

@section{Examples}
 


@racketblock[

(
SynthDef("help-SinOsc", { arg freq=440, out;
	Out.ar(out, SinOsc.ar(freq, 0, 0.1))
}).add;

SynthDef("help-XOut", { arg out=0, xFade=1;
	var source;
		source = PinkNoise.ar(0.05);

		// write to the bus, crossfading with previous contents
		XOut.ar(out, xFade, source);

}).add;
)

Synth("help-SinOsc", [\freq, 500]);
a = Synth.tail(s, "help-XOut");


a.set(\xFade, 0.7);
a.set(\xFade, 0.4);
a.set(\xFade, 0.0);

::

]


