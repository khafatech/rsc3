#lang scribble/manual
@(require (for-label racket))

@title{LFPulse}
pulse oscillator@section{categories}
 UGens>Generators>Deterministic
@section{related}
 Classes/LFSaw

@section{description}

A non-band-limited pulse oscillator. Outputs a high value of one and a low value of zero.

@section{classmethods}
 

@section{method}
 ar, kr

@section{argument}
 freq
frequency in Hertz

@section{argument}
 iphase
initial phase offset in cycles ( 0..1 )

@section{argument}
 width
pulse width duty cycle from zero to one.

@section{argument}
 mul

@section{argument}
 add

@section{instancemethods}
 
@section{private}
 signalRange

@section{examples}
 

a plot:

@racketblock[
{ LFPulse.ar(Line.kr(100, 800, 0.1)) }.plot(0.1);
::

50 Hz wave:
]

@racketblock[
{ LFPulse.ar(50) * 0.1 }.play;
::

modulating frequency:
]

@racketblock[
{ LFPulse.ar(XLine.kr(1, 200, 10), 0, 0.2, 0.1) }.play;
::

amplitude modulation:
]

@racketblock[
{ LFPulse.kr(XLine.kr(1, 200, 10), 0, 0.2) * SinOsc.ar(440) * 0.1 }.play;
::

used as both Oscillator and LFO:
]

@racketblock[
{ LFPulse.ar(LFPulse.kr(3, 0, 0.3, 200, 200), 0, 0.2, 0.1) }.play;
::

scope:
]

@racketblock[
{ LFPulse.ar(500, 0, MouseX.kr, 0.5) }.scope;
::

compare with band limited Pulse UGen:
]

@racketblock[
(
{
    [
        Pulse.ar(100, 0.3, 0.5),
        LFPulse.ar(100, 0, 0.3, 0.5)
    ] * 0.2
}.scope(bufsize: 44100, zoom: 5)
)
::

]


