#lang scribble/manual
@(require (for-label racket))

@title{Klang}
 Sine oscillator bank@section{related}
  Classes/Klank, Classes/DynKlang
@section{categories}
  UGens>Generators>Deterministic

@section{description}

Klang is a bank of fixed frequency sine oscillators. Klang is more
efficient than creating individual oscillators but offers less
flexibility.


@section{classmethods}
 

@section{method}
 ar

@section{argument}
 specificationsArrayRef
A link::Classes/Ref:: to an link::Classes/Array:: of three Arrays:

@section{definitionlist}
 
## frequencies: || An Array of oscillator frequencies.
## amplitudes: || an Array of oscillator amplitudes, or nil. If nil, then amplitudes default to 1.0.
## phases: || an Array of initial phases, or nil. If nil, then phases default to 0.0.
::

@section{argument}
 freqscale
A scale factor multiplied by all frequencies at initialization time.

@section{argument}
 freqoffset
An offset added to all frequencies at initialization time.

@section{discussion}
 
The parameters in 
@racketblock[specificationsArrayRef:: can't be changed after it has been started.
For a modulatable but less efficient version, see link::Classes/DynKlang::.


]
@section{Examples}
 


@racketblock[
play({ Klang.ar(`[ [800, 1000, 1200], [0.3, 0.3, 0.3], [pi, pi, pi]], 1, 0) * 0.4});

play({ Klang.ar(`[ [800, 1000, 1200], nil, nil], 1, 0) * 0.25});

play({ Klang.ar(`[ Array.rand(12, 600.0, 1000.0), nil, nil ], 1, 0) * 0.05 });



(
{
loop({
	play({
		Pan2.ar(Klang.ar(`[ Array.rand(12, 200.0, 2000.0), nil, nil ], 1, 0), 1.0.rand)
			 * EnvGen.kr(Env.sine(4), 1, 0.02, doneAction: Done.freeSelf);
	});
	2.wait;
})
}.fork;
)

// Multichannel expansion:

(
{
loop({
	play({
		var nPartials = 12, nChans = 5, n = nPartials * nChans;
		Splay.ar(Klang.ar(`[ { { rrand(200.0, 2000.0) } ! nPartials } ! nChans, nil, nil ], 1, 0))
			 * EnvGen.kr(Env.sine(4), 1, 0.02, doneAction: Done.freeSelf);
	});
	2.wait;
})
}.fork;
)
::


]


