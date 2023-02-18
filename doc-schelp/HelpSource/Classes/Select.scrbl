#lang scribble/manual
@(require (for-label racket))

@title{Select}
 Select output from an array of inputs.@section{categories}
  UGens>Multichannel>Select
@section{related}
  Classes/SelectX, Classes/SelectXFocus, Classes/LinSelectX

@section{description}

The output is selected from an array of inputs.

@section{note}
  All the UGens are continuously running. This may not be the most efficient
way if each input is CPU-expensive. ::

Note that the array is fixed at the time of writing the SynthDef, and the
whole array is embedded in the SynthDef file itself.  For small arrays
this is more efficient than reading from a buffer.


@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 which

Integer index


@section{argument}
 array

Input array of signals


@section{Examples}
 


@racketblock[
(
SynthDef("help-Select",{ arg out=0;

	var a,cycle;
	a = [
			SinOsc.ar,
			Saw.ar,
			Pulse.ar
		];
	cycle = a.size  * 0.5;
	Out.ar(out,
		Select.ar(LFSaw.kr(1.0,0.0,cycle,cycle),a) * 0.2
	)
}).play;

)

//Here used as a sequencer:
(
SynthDef("help-Select-2",{ arg out=0;

	var a,s,cycle;
	a = Array.fill(32,{ rrand(30,80) }).midicps;
	a.postln;
	cycle = a.size  * 0.5;

	s = Saw.ar(
			Select.kr(
				LFSaw.kr(1.0,0.0,cycle,cycle),
				a
			),
			0.2
	);
	Out.ar(out,s )
}).play;
)

::

]


