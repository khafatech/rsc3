#lang scribble/manual
@(require (for-label racket))

@title{DynKlang}
@section{categories}
  UGens>Generators>Deterministic
 Dynamic sine oscillator bank@section{related}
  Classes/Klang, Classes/DynKlank, Classes/Klank

@section{description}

DynKlang is a bank of sine oscillators. It is less efficient than Klang, as it is basically a wrapper around SinOsc UGens in order to provide a similar interface to link::Classes/Klang::.

Unlike Klang, parameters in 
@racketblock[specificationsArrayRef:: can be changed after it has been started.

]
@section{classmethods}
 
@section{private}
  categories

@section{method}
  ar, kr

@section{argument}
  specificationsArrayRef
a Ref to an Array of three Arrays:
@section{definitionlist}
 
## frequencies || an Array of oscillator frequencies.
## amplitudes || an Array of oscillator amplitudes, or nil. If nil, then amplitudes default to 1.0
## phases || an Array of initial phases, or nil. If nil, then phases default to 0.0
::
@section{argument}
  freqscale
a scale factor multiplied by all frequencies at initialization time.
@section{argument}
  freqoffset
an offset added to all frequencies at initialization time.

@section{examples}
 

@racketblock[
// frequency modulation
(
play {
	DynKlang.ar(`[
		[800, 1000, 1200] + SinOsc.kr([2, 3, 4.2], 0, [13, 24, 12]),
		[0.3, 0.3, 0.3],
		[pi,pi,pi]
	]
) * 0.1
};
)


// building new synths every 2 seconds
(
{
loop({
	play({
		var mod = SinOsc.kr(Rand(0.1, 0.9), 0, Rand(5, 20));
		Pan2.ar(DynKlang.ar(`[ Array.rand(12, 200.0, 2000.0), 1, mod ]), 1.0.rand)
			* EnvGen.kr(Env.sine(4), 1, 0.02, doneAction: Done.freeSelf);
	});
	2.wait;
})
}.fork;
)


// resetting the frequencies and amplitudes after the synth has been created
(
SynthDef('help-dynKlang', {| freqs=#[220, 440, 880, 1760],
	amps=#[0.35, 0.23, 0.12, 0.05],
	phases=#[1, 1.5, 2, 2.5]|

	Out.ar(0, DynKlang.ar(`[freqs, amps, phases]))
}).add
)

a = Synth('help-dynKlang');

a.setn(\freqs, Array.rand(4, 500, 2000));
a.setn(\amps, Array.rand(4, 0.01, 0.25));
::

]


