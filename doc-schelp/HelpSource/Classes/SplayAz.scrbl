#lang scribble/manual
@(require (for-label racket))

@title{SplayAz}
 Spreads an array of channels across a ring of channels@section{categories}
  UGens>Multichannel>Panners
@section{related}
  Classes/Splay, Classes/PanAz, Classes/SelectXFocus

@section{description}

SplayAz spreads an array of channels across a ring of channels.
Optional spread and center controls, and equal power levelCompensation.
numChans and orientation are as in link::Classes/PanAz::.


@racketblock[
{ SplayAz.ar(5, [SinOsc.ar, Saw.ar], 0, 1) }.plot;
::

]
@section{classmethods}
 

@section{method}
  ar, kr

Each of the inputs is evenly spaced over a cyclic period of
2.0 in pos with 0.0 equal to channel zero and 2.0/numChans equal
to channel 1, 4.0/numChans equal to channel 2, etc.

The distance between the input signals in the output range is determined by the spread argument.

@section{argument}
  numChans
Number of output channels of the UGen
@section{argument}
  inArray
Input signals (can be a single UGen or an array)
@section{argument}
  spread
How far the input signals  are apart in the output. If zero, everything is mixed on center position (see below).

@racketblock[
{ SplayAz.ar(6, [SinOsc.ar, Saw.ar(800)], spread: MouseX.kr(0, 1).poll) * 0.3 }.scope;
::

]
@section{argument}
  level
Scaling for all signals
@section{argument}
  width
Over how much of the channels each signal is distributed.
@section{argument}
  center
Which of the channels will be the first channel
@section{argument}
  orientation
Should be zero if the front is a vertex of the polygon. The first
speaker will be directly in front. Should be 0.5 if the front
bisects a side of the polygon. Then the first speaker will be the
one left of center.
@section{argument}
  levelComp
If true, the signal level is adjusted to maintain overall loudness the same (n.reciprocal.sqrt).

@section{method}
  arFill
@section{argument}
  numChans
Number of output channels
@section{argument}
  n
Number of input channels
@section{argument}
  function
A function that returns a UGen (the channel index is passed as an argument)
@section{argument}
  spread
@section{argument}
  level
@section{argument}
  width
@section{argument}
  center
@section{argument}
  orientation
@section{argument}
  levelComp

@section{examples}
 

@racketblock[
(
x = { arg spread=1, level=0.2, width=2, center=0.0;
 SplayAz.ar(
  4,
  SinOsc.ar( { |i| LFNoise2.kr( rrand(10, 20), 200, i + 3 * 100) } ! 10),
  spread,
  level,
  width,
  center
 );
}.scope;
)

x.set(\spread, 1,   \center, 0);  // full n chans
x.set(\spread, 0.5, \center, -0.25); // less wide
x.set(\spread, 0, \center, 0);  // mono center (depends on orientation, see PanAz)
x.set(\spread, 0, \center, -0.25); //
x.set(\spread, 0.0, \center, 0.5); // mono, but rotate 1 toward the higher channels
x.set(\spread, 0.5, \center, 0.5); // spread over the higher channels
x.set(\spread, 0,   \center, -0.25); // all on first channel
x.set(\spread, 1,   \center, 0);  // full n chans

x.free;

 // the same example written with arFill:
(
x = { arg spread=1, level=0.5, width=2, center=0.0;
 SplayAz.arFill(
  4,
  10,
  { |i| SinOsc.ar( LFNoise2.kr( rrand(10, 20), 200, i + 3 * 100) ) },
  spread,
  level,
  width,
  center
 );
}.scope;
)

 // or with mouse control
(
x = { var src;
 src = SinOsc.ar( { |i| LFNoise2.kr( rrand(10, 20), 200, i * 100 + 400) } ! 10);
 SplayAz.ar(4, src, MouseY.kr(1, 0), 0.2, center: MouseX.kr(-1, 1));
}.scope;
)

// test for correct behavior:
	// only on chan 0
{ SplayAz.ar(4, SinOsc.ar * 0.2, orientation: 0) }.scope;

	//  on chan 0, 3, i.e. equally around the ring
{ SplayAz.ar(6, SinOsc.ar([2, 3] * 200) * 0.2, orientation: 0) }.scope;

	// equal spread on 0, 2, 4
{ SplayAz.ar(6, SinOsc.ar([2, 3, 5] * 200) * 0.2, orientation: 0) }.scope;


	// wrong behavior of SplayZ:
		// plays on chan 2, but should play on 0
{ SplayZ.ar(4, SinOsc.ar * 0.2, orientation: 0) }.scope;

	//  wrong: mixes both to chan 2,
	// because pan values [-1, 1] are the same pos on the ring
{ SplayZ.ar(6, SinOsc.ar([2, 3] * 200) * 0.2, orientation: 0) }.scope;

	// wrong equal spread to pan values [-1, 0, 1], which outputs to chans 2, 0, 2
{ SplayZ.ar(6, SinOsc.ar([2, 3, 5] * 200) * 0.2, orientation: 0) }.scope;
::
]


