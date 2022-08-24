#lang scribble/manual
@(require (for-label racket))

@title{PV_ConformalMap}
 Complex plane attack.@section{related}
  Classes/FFT, Classes/IFFT
@section{categories}
  UGens>FFT

@section{description}


Applies the conformal mapping


@racketblock[

z → (z - a) / (1 - za*)

::

to the phase vocoder bins z with a given by the real and imag inputs to
the UGen.


Makes a transformation of the complex plane so the output is full of
phase vocoder artifacts but may be musically fun. Usually keep

]

@racketblock[
|a| < 1
::
but
you can of course try bigger values to make it really noisy.

]

@racketblock[
a = 0
::
should
give back the input mostly unperturbed.


See  link::http://mathworld.wolfram.com/ConformalMapping.html:: .


]
@section{classmethods}
 

@section{method}
 new

@section{argument}
 buffer

FFT buffer.


@section{argument}
 areal

Real part of a.


@section{argument}
 aimag

Imaginary part of a.


@section{Examples}
 


@racketblock[

//explore the effect
(
SynthDef("conformer1", {
	var in, chain;
	in = SoundIn.ar(0, 0.5);
	chain = FFT(LocalBuf(1024), in);
	chain=PV_ConformalMap(chain, MouseX.kr(-1.0,1.0), MouseY.kr(-1.0,1.0));
	Out.ar(0, Pan2.ar(IFFT(chain),0));
}).add;
)

a = Synth("conformer1")
a.free

(
SynthDef("conformer2", {
	var in, chain, out;
	in = Mix.ar(LFSaw.ar(SinOsc.kr(Array.rand(3,0.1,0.5),0,10,[1,1.1,1.5,1.78,2.45,6.7]*220),0,0.3));
	chain = FFT(LocalBuf(2048), in);
	chain=PV_ConformalMap(chain, MouseX.kr(0.01,2.0, 'exponential'), MouseY.kr(0.01,10.0, 'exponential'));
	out=IFFT(chain);

	Out.ar(0, Pan2.ar(CombN.ar(out, 0.1, 0.1, 10, 0.5, out), 0, 0.3));
}).add;
)

a = Synth("conformer2")
a.free
::

]


