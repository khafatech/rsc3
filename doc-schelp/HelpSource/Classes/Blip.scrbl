#lang scribble/manual
@(require (for-label racket))

@title{Blip}
 Band limited impulse oscillator.@section{related}
  Classes/Impulse
@section{categories}
   UGens>Generators>Deterministic


@section{description}


Band Limited ImPulse generator. All harmonics have equal amplitude.
This is the equivalent of 'buzz' in  emphasis::MusicN:: languages.

emphasis::Synth-O-Matic:: (1990) had an impulse generator
called blip, hence that name here rather than 'buzz'.

It is improved from other implementations in that it will crossfade
in a control period when the number of  harmonics changes,
so that there are no audible pops. It also eliminates the divide in
the formula by using a 1/sin table (with special precautions taken for
1/0).  The lookup tables are linearly interpolated for better quality.

@section{warning}
 
This waveform in its raw form could be damaging to your ears at high
amplitudes or for long periods.
::

@section{classmethods}
 

@section{method}
 ar

@section{argument}
 freq
Frequency in Hertz.

@section{argument}
 numharm
Number of harmonics. This may be lowered internally if it would cause aliasing.

@section{argument}
 mul

@section{argument}
 add

@section{Examples}
 


@racketblock[
// modulate frequency
{ Blip.ar(XLine.kr(20000,200,6),100,0.2) }.play;

// modulate numharmonics
{ Blip.ar(200,Line.kr(1,100,20),0.2) }.play;
::

]


