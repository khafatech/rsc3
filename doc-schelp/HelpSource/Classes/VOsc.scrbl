#lang scribble/manual
@(require (for-label racket))

@title{VOsc}
 Variable wavetable oscillator.@section{related}
  Classes/COsc, Classes/Osc, Classes/OscN, Classes/VOsc3
@section{categories}
   UGens>Generators>Deterministic


@section{description}


A wavetable lookup oscillator which can be swept smoothly across
wavetables. All the wavetables must be allocated to the same size.
Fractional values of table will interpolate between two adjacent tables.


This oscillator requires at least two buffers to be filled with a
wavetable format signal. This preprocesses the Signal into a form which
can be used efficiently by the Oscillator. The buffer size must be a
power of 2.


This can be achieved by creating a link::Classes/Buffer:: object and sending it one of
the "b_gen" messages ( sine1, sine2, sine3 ) with the wavetable flag set to true.


This can also be achieved by creating a link::Classes/Signal:: object and sending it
the link::Overviews/Methods#asWavetable#asWave@section{table}
  message, saving it to disk, and having the server load
it from there.


If you use Buffer objects to manage buffer numbers, you can use the
[*allocConsecutive] method to allocate a continuous block of buffers.
See the link::Classes/Buffer:: helpfile for details.


@section{note}
 

VOsc requires the b_gen sine1 wavetable flag to be ON.

::

@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 bufpos

Buffer index. Can be swept continuously among adjacent wavetable
buffers of the same size.


@section{argument}
 freq

Frequency in Hertz.


@section{argument}
 phase

Phase offset or modulator in radians.


@section{argument}
 mul

Output will be multiplied by this value.


@section{argument}
 add

This value will be added to the output.


@section{Examples}
 


@racketblock[

(
s = Server.local;
// allocate and fill tables 0 to 7
8.do({ arg i;
	var n, a;
	// allocate table
	s.sendMsg(\b_alloc, i, 1024);
	// generate array of harmonic amplitudes
	n = (i+1)**2;
	a = Array.fill(n, { arg j; ((n-j)/n).squared.round(0.001) });
	// fill table
	s.performList(\sendMsg, \b_gen, i, \sine1, 7, a);
	// the argument '7' here is a flag for the \sine1 wave fill method -
	// see the "Wave Fill Commands" section in the Server Command Reference 
});
)

(
SynthDef("help-VOsc",{ arg out=0, bufoffset=0;
	var x;
	// mouse x controls the wavetable position
	x = MouseX.kr(0,7);
	Out.ar(out,
		VOsc.ar(bufoffset+x, [120,121], 0, 0.3)
	)
}).play(s,[\out, 0, \bufoffset, 0]);
)

(
8.do({ arg i;
	var a;
	s.sendMsg(\b_alloc, i, 1024); // allocate table
	// generate array of harmonic amplitudes
	a = Array.fill(i, 0) ++ [0.5, 1, 0.5];
	// fill table
	s.performList(\sendMsg, \b_gen, i, \sine1, 7, a);
});
)

(
8.do({ arg i;
	var a;
	s.sendMsg(\b_alloc, i, 1024); // allocate table
	// generate array of harmonic amplitudes
	a = Array.fill(32,0);
	12.do({ arg i; a.put(32.rand, 1) });
	// fill table
	s.performList(\sendMsg, \b_gen, i, \sine1, 7, a);
});
)

(
8.do({ arg i;
	var a;
	s.sendMsg(\b_alloc, i, 1024); // allocate table
	// generate array of harmonic amplitudes
	n = (i+1)**2;
	a = Array.fill(n, { arg j; 1.0.rand2 });
	// fill table
	s.performList(\sendMsg, \b_gen, i, \sine1, 7, a);
});
)

::

]


