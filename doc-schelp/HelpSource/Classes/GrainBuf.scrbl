#lang scribble/manual
@(require (for-label racket))

@title{GrainBuf}
 Granular synthesis with sound stored in a buffer@section{categories}
  UGens>Buffer, UGens>Generators>Granular
@section{related}
  Classes/GrainIn, Classes/GrainFM, Classes/GrainSin

@section{classmethods}
 
@section{private}
  categories

@section{method}
  ar

@section{argument}
  numChannels
the number of channels to output. If 1, mono is returned and pan is ignored.

@section{argument}
  trigger
a kr or ar trigger to start a new grain. If ar, grains after the start of the synth are sample accurate.

@section{argument}
  dur
size of the grain (in seconds).

@section{argument}
  sndbuf
the buffer holding a mono audio signal. If using multi-channel files, use Buffer.readChannel.

@section{argument}
  rate
the playback rate of the sampled sound

@section{argument}
  pos
the playback position for the grain to start with (0 is beginning, 1 is end of file)

@section{argument}
  interp
the interpolation method used for pitchshifting grains:
@section{list}
 
## 1 = no interpolation
## 2 = linear
## 4 = cubic interpolation (more computationally intensive)
::

@section{argument}
  pan
determines where to pan the output.
@section{list}
 
## If numChannels = 1, no panning is done.
## If numChannels = 2, panning is similar to Pan2.
## If numChannels > 2, panning is the same as PanAz.
::

@section{argument}
  envbufnum
the buffer number containing a signal to use for the grain envelope. -1 uses a built-in Hann envelope.

@section{argument}
  maxGrains
the maximum number of overlapping grains that can be used at a given time. This value is set at the UGens init time and can't be modified. Defaults to 512. This can be set lower for more efficient use of memory.

@section{warning}
  The above parameter is new (post SC 3.3.1) and has the potential to break code written <= 3.3.1. This parameter is BEFORE the mul slot, and you may need to update code to account for this difference. ::

@section{argument}
  mul

@section{argument}
  add

@section{discussion}
  All args except numChannels and trigger are polled at grain creation time.
@section{instancemethods}
 
@section{private}
  init, argNamesInputsOffset

@section{examples}
 

@racketblock[
s.boot;

(
var winenv;

b = Buffer.read(s, Platform.resourceDir +/+ "sounds/a11wlk01-44_1.aiff");
// a custom envelope
winenv = Env([0, 1, 0], [0.5, 0.5], [8, -8]);
z = Buffer.sendCollection(s, winenv.discretize, 1);

SynthDef(\buf_grain_test, {arg gate = 1, amp = 1, sndbuf, envbuf;
	var pan, env, freqdev;
	// use mouse x to control panning
	pan = MouseX.kr(-1, 1);
	env = EnvGen.kr(
		Env([0, 1, 0], [1, 1], \sin, 1),
		gate,
		levelScale: amp,
		doneAction: Done.freeSelf);
	Out.ar(0,
		GrainBuf.ar(2, Impulse.kr(10), 0.1, sndbuf, LFNoise1.kr.range(0.5, 2),
			LFNoise2.kr(0.1).range(0, 1), 2, pan, envbuf) * env)
	}).add;

)

// use built-in env
x = Synth(\buf_grain_test, [\sndbuf, b, \envbuf, -1])

// switch to the custom env
x.set(\envbuf, z)
x.set(\envbuf, -1);

x.set(\gate, 0);
::

]


