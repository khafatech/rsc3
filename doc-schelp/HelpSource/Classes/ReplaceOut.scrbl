#lang scribble/manual
@(require (for-label racket))

@title{ReplaceOut}
 Send signal to a bus, overwriting previous contents.@section{related}
  Classes/OffsetOut, Classes/Out, Classes/XOut
@section{categories}
   UGens>InOut


@section{description}

link::Classes/Out::  adds it's output to a given bus, making it
available to all nodes later in the node tree (See Synth and
Order-of-execution for more information). ReplaceOut overwrites those
contents. This can make it useful for processing.


See the link::Reference/Server-Architecture:: and link::Classes/Bus:: helpfiles for more information on
buses and how they are used.


@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 bus

The index of the bus to write out to. The lowest numbers are
written to the audio hardware.


@section{argument}
 channelsArray

An Array of channels or single output to write out. You cannot
change the size of this once a SynthDef has been built.


@section{Examples}
 


@racketblock[

(
SynthDef("ReplaceOutHelp", { arg out=0, freq=440;
	var source;
		source = SinOsc.ar(freq, 0, 0.1);

		// write to the bus, replacing previous contents
		ReplaceOut.ar(out, source);

}).add;
)

// each Synth replaces the output of the previous one
x = Synth.tail(s, "ReplaceOutHelp", [\freq, 500]);
y = Synth.tail(s, "ReplaceOutHelp", [\freq, 600]);
z = Synth.tail(s, "ReplaceOutHelp", [\freq, 700]);

// release them in reverse order; the older Synths are still there.
z.free;
y.free;
x.free;

::

]


