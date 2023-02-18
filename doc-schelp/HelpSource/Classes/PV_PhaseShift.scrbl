#lang scribble/manual
@(require (for-label racket))

@title{PV_PhaseShift}
 Shift phase.@section{related}
  Classes/FFT, Classes/IFFT, Classes/PV_PhaseShift90, Classes/PV_PhaseShift270, Classes/PV_Diffuser
@section{categories}
  UGens>FFT

@section{description}


Shift phase of all bins.


@section{classmethods}
 

@section{method}
 new

@section{argument}
 buffer

FFT buffer.


@section{argument}
 shift

Phase shift in radians.

@section{argument}
 integrate

If greater than zero, integrate the phase-shift across calls (for an accumulating phase shift).

@section{Examples}
 


@racketblock[

s.boot;

(
SynthDef("help-phaseShift", { arg out=0;
        var in, chain;
        in = SinOsc.ar(500);
        chain = FFT(LocalBuf(2048), in);
        chain = PV_PhaseShift(chain, LFNoise2.kr(1, 180, 180));
        Out.ar(out, 0.1 * IFFT(chain).dup);
}).play(s);
)
::

]


