#lang scribble/manual
@(require (for-label racket))

@title{ProxySynthDef}
 synth def that wraps ugen graph@section{categories}
  Libraries>JITLib>NodeProxy
@section{related}
  Classes/NodeProxy

@section{description}

(used internally by link::Classes/NodeProxy::)

for inner workings see link::Tutorials/JITLib/jitlib_fading::

@section{ClassMethods}
 

@section{method}
 new

@section{argument}
 name
like in link::Classes/SynthDef::. todo: add variants.

@section{argument}
 func
like in link::Classes/SynthDef::. todo: add variants.

@section{argument}
 rates
like in link::Classes/SynthDef::. todo: add variants.

@section{argument}
 prependArgs
like in link::Classes/SynthDef::. todo: add variants.

@section{argument}
 makeFadeEnv
if true it constructs a fader envelope and adds controls for gate and fadeTime

@section{argument}
 channelOffset
a constant offset that is added to the out number

@section{argument}
 chanConstraint
max numChannels for the synthdef. If ugenfunc returns a larger array, it wraps

@section{argument}
 rateConstraint
a symbol like \audio, \control or \scalar.

@section{method}
 sampleAccurate
always use link::Classes/OffsetOut::, if set to true (default: false)

@section{Examples}
 


@racketblock[
a = ProxySynthDef("xtest", { SinOsc.ar(400) * 0.1 });

a.add;

x = Synth("xtest");
x.release;


/*

	if the resulting number of channels is larger than a given channelConstraint,
	it behaves according to the rate: audio rate signals are wrapped around
	a smaller channel size, control rate signals are not (the exceeding channels are left out)

*/
::
]


