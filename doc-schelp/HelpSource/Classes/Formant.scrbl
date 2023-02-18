#lang scribble/manual
@(require (for-label racket))

@title{Formant}
 Formant oscillator@section{categories}
   UGens>Generators>Deterministic


@section{description}


Generates a set of harmonics around a formant frequency at a given
fundamental frequency.


@section{classmethods}
 

@section{method}
 ar

@section{argument}
 fundfreq
Fundamental frequency in Hertz. (control rate)

@section{argument}
 formfreq
Formant frequency in Hertz. (control rate)

@section{argument}
 bwfreq
Pulse width frequency in Hertz. Controls the bandwidth of the
formant. (control rate)

Must be greater than or equal to 
@racketblock[fundfreq::.

]
@section{argument}
 mul

@section{argument}
 add

@section{discussion}
 
The frequency inputs are read at control rate only, so if you use an audio rate UGen as an input, it will only be sampled at the start of each audio synthesis block.


@section{Examples}
 


@racketblock[
// modulate fundamental frequency, formant freq stays constant
{ Formant.ar(XLine.kr(400,1000, 8), 2000, 800, 0.125) }.play

// modulate formant frequency, fundamental freq stays constant
{ Formant.ar(200, XLine.kr(400, 4000, 8), 200, 0.125) }.play

// modulate width frequency, other freqs stay constant
{ Formant.ar(400, 2000, XLine.kr(800, 8000, 8), 0.125) }.play
::
]


