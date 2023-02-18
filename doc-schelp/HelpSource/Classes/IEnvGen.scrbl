#lang scribble/manual
@(require (for-label racket))

@title{IEnvGen}
 Envelope generator for polling values from an Env@section{categories}
  UGens>Envelopes
@section{related}
  Classes/Env

@section{description}

Envelope generator for polling values from an envelope. IEnvGen plays back break point envelopes from the 
@racketblock[index:: point. The envelopes are instances of the link::Classes/Env:: class.

]
@section{classmethods}
 
@section{private}
  categories, new1

@section{method}
  ar, kr

@section{argument}
  envelope
an instance of Env (this is static for the life of the UGen)
@section{argument}
  index
a point to access within the Env
@section{argument}
  mul
@section{argument}
  add

@section{instancemethods}
 
@section{private}
  init, argNamesInputsOffset, convertEnv

@section{examples}
 


@racketblock[
(
{
	var env =  Env([0, 0.6, 0.3, 1.0, 0], [0.1, 0.02, 0.4, 1.1], [\lin, \exp, -6, \sin]);
	var envgen = IEnvGen.kr(env, MouseX.kr(0, env.times.sum));
	env.plot;
	SinOsc.ar(envgen * 500 + 440)
}.play;
)

// index with an SinOsc ... mouse controls amplitude of SinOsc
// use offset so negative values of SinOsc will map into the Env
(
{
	var sin = SinOsc.ar(440, 0, MouseX.kr(0, 1));
	// use offset so negative values of SinOsc will map into the Env
	var env = Env([-1, -0.7, 0.7, 1], [ 0.8666, 0.2666, 0.8668 ], \lin, offset: -1.0);
	IEnvGen.ar(env, sin) * 0.1
}.play;
)

// index with Amplitude of input, control freq of SinOsc (uses SoundIn)
(
{
	var point = Amplitude.ar(SoundIn.ar(0), 0.01, 0.2);
	// use offset so negative values of SinOsc will map into the Env
	var env = Env.xyc([[0, 330, \exp], [0.5, 440, \exp], [1.0, 1760]]);
	SinOsc.ar(IEnvGen.kr(env, point)) * 0.2
}.play;
)

::

]


