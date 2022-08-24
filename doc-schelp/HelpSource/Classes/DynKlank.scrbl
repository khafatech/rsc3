#lang scribble/manual
@(require (for-label racket))

@title{DynKlank}
 Bank of resonators.@section{related}
  Classes/Klang, Classes/Klank
@section{categories}
   UGens>Generators>Deterministic, UGens>Filters>Linear


@section{description}


DynKlank is a bank of frequency resonators which can be used to simulate
the resonant modes of an object. Each mode is given a ring time, which is
the time for the mode to decay by 60 dB.


Unlike  link::Classes/Klank::, all parameters in DynKlank can be changed in real-time after it has been started.

@section{Note}
 
The amplitude of the resulting signal depends on the server's sample rate. See link::Classes/Ringz#Interaction with sample rate#Ringz: Interaction with sample rate:: for details.
::


@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 specificationsArrayRef
A Ref to an Array of three Arrays: 
@racketblock[[frequencies, amplitudes, ringtimes]::

]
@section{definitionlist}
 
## frequencies: || An Array of filter frequencies.
## amplitudes: || An Array of filter amplitudes, or nil. If nil, then amplitudes default to 1.0.
## ring times: || An Array of 60 dB decay times for the filters.
::
All subarrays, if not nil, should have the same length.

@section{argument}
 input
The excitation input to the resonant filter bank.

@section{argument}
 freqscale
A scale factor multiplied by all frequencies at initialization time.

@section{argument}
 freqoffset
An offset added to all frequencies at initialization time.

@section{argument}
 decayscale
A scale factor multiplied by all ring times at initialization time.

@section{Examples}
 

Four resonators each at maximum amplitude of 1.0 and ring times of 1 second, different exciters and no scaling:
@section{Note}
  Watch the ` before the opening bracket of the parameter array! Also see link::Guides/Multichannel-Expansion::::

@racketblock[

{ DynKlank.ar(`[[800, 1071, 1153, 1723], nil, [1, 1, 1, 1]], Impulse.ar(2, 0, 0.1)) }.play;

{ DynKlank.ar(`[[800, 1071, 1353, 1723], nil, [1, 1, 1, 1]], Dust.ar(8, 0.1)) }.play;

{ DynKlank.ar(`[[800, 1071, 1353, 1723], nil, [1, 1, 1, 1]], PinkNoise.ar(0.007)) }.play;

{ DynKlank.ar(`[[200, 671, 1153, 1723], nil, [1, 1, 1, 1]], PinkNoise.ar([0.007,0.007])) }.play;

::

Changing parameters in realtime:
]

@racketblock[
(
// change freqs and ringtimes with mouse
{	var freqs, ringtimes;
	freqs = [800, 1071, 1153, 1723] * MouseX.kr(0.5, 2, 1);
	ringtimes = [1, 1, 1, 1] * MouseY.kr(0.1, 10, 1);
	DynKlank.ar(`[freqs, nil, ringtimes ], Impulse.ar(2, 0, 0.1))
}.play;
)

(
// set them from outside later:
SynthDef('help-dynKlank', {
	var freqs, ringtimes, signal;
	freqs = Control.names([\freqs]).kr([800, 1071, 1153, 1723]);
	ringtimes = Control.names([\ringtimes]).kr([1, 1, 1, 1]);
	signal = DynKlank.ar(`[freqs, nil, ringtimes ], Impulse.ar(2, 0, 0.1));
	Out.ar(0, signal);
}).add;
)

a = Synth('help-dynKlank');

a.setn(\freqs, Array.rand(4, 500, 2000));
a.setn(\ringtimes, Array.rand(4, 0.2, 4) );

// create multichannel controls directly with literal arrays:
(
SynthDef('help-dynKlank', {|
	freqs (#[100, 200, 300, 400]),
	amps (#[1, 0.3, 0.2, 0.05]),
	rings (#[1, 1, 1, 2])|

	Out.ar(0, DynKlank.ar(`[freqs, amps, rings], WhiteNoise.ar * 0.001))
}).add
)

a = Synth('help-dynKlank');

a.setn(\freqs, Array.rand(4, 500, 2000));
a.setn(\amps, Array.exprand(4, 0.01, 1));

{ Out.kr(102, MouseX.kr(1, 2) * Array.rand(4, 500, 2000)) }.play;
a.mapn(\freqs, 102, 4);
::


]


