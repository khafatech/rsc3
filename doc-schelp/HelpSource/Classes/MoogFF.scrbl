#lang scribble/manual
@(require (for-label racket))

@title{MoogFF}
 Moog VCF implementation, designed by Federico Fontana@section{categories}
  UGens>Filters
@section{related}
  Classes/RLPF, Classes/LPF

@section{description}

A digital implementation of the Moog VCF (filter). @section{footnote}
 
The design of this filter is described in the conference paper Fontana, F. (2007) emphasis::Preserving the Digital Structure of the Moog VCF::. In Proc. ICMC07, Copenhagen, 25-31 August 2007.
::
@section{footnote}
 
Original Java code created by F. Fontana - August 2007 - federico.fontana@univr.it
Ported to C++ for SuperCollider by Dan Stowell
::

@section{classmethods}
 
@section{method}
  ar, kr

@section{argument}
  in
the input signal.
@section{argument}
  freq
the cutoff frequency.
@section{argument}
  gain
the filter resonance gain, between zero and 4.
@section{argument}
  reset
when greater than zero, this will reset the state of the digital filters at the beginning of a computational block.
@section{argument}
  mul
@section{argument}
  add

@section{examples}
 

@racketblock[
s.boot;
// Play it with the mouse...
x = { MoogFF.ar(WhiteNoise.ar(01.1), MouseY.kr(100, 10000, 1), MouseX.kr(0, 4)) }.play(s);
x.free;

// Mmmm, throbby
(
x = {
    MoogFF.ar(
        Pulse.ar([40,121], [0.3,0.7]),
        SinOsc.kr(LFNoise0.kr(0.42).range(0.001, 2.2)).range(30, 4200),
        0.83 * 4)}.play(s);
)
x.free;
::

]


