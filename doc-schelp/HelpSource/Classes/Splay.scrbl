#lang scribble/manual
@(require (for-label racket))

@title{Splay}
 Splay spreads an array of channels across the stereo field@section{categories}
  UGens>Multichannel>Panners
@section{related}
  Classes/SplayAz, Classes/SplayZ

@section{description}

Splay spreads an array of channels across the stereo field.
Optional arguments are spread and center, and equal power levelCompensation.
The formula for the stereo position is ((0 .. (n - 1)) * (2 / (n - 1) - 1) * spread + center

@section{classmethods}
 
@section{method}
  ar, kr
@section{argument}
  inArray
The array of channels to be distributed over the two stereo pairs
@section{argument}
  spread
For spread = 0, all channels end up in the centre, for 1, they have maximum distribution
@section{argument}
  level
An amplitude multiplier for all channels
@section{argument}
  center
Shift the centre of the distribution.
@section{argument}
  levelComp


@section{method}
  arFill
In analogy to Mix:arFill, this method takes a function that produces the channels. The counting index is passed to it.
@section{argument}
  n
Number of channels
@section{argument}
  function
Function to return each channel
@section{argument}
  spread
For spread = 0, all channels end up in the centre, for 1, they have maximum distribution
@section{argument}
  level
An amplitude multiplier for all channels
@section{argument}
  center
Shift the centre of the distribution.
@section{argument}
  levelComp

@section{examples}
 


@racketblock[
(
x = { arg spread=1, level=0.2, center=0.0;
 Splay.ar(
  SinOsc.ar( { |i| LFNoise2.kr(1).exprange(200, 4000) } ! 10),
  spread,
  level,
  center
 );
}.play;
)

x.set(\spread, 1,   \center, 0);  // full stereo
x.set(\spread, 0.5, \center, 0);  // less wide
x.set(\spread, 0,   \center, 0);  // mono center
x.set(\spread, 0.5, \center, 0.5);
// spread from center to right
x.set(\spread, 0,   \center, -1); // all left
x.set(\spread, 1,   \center, 0);  // full stereo


 // the a similar example written with arFill:
(
x = { arg spread=1, level=0.2, center=0.0;
 Splay.arFill(10,
  { |i| SinOsc.ar( LFNoise2.kr( rrand(10, 20), 200, i + 3 * 100))  },
  spread,
  level,
  center
 );
}.play;
)


 // with mouse control
(
x = { var src;
 src = SinOsc.ar( { |i| LFNoise2.kr( rrand(10, 20), 200, i + 3 * 100) } ! 10);
 Splay.ar(src, MouseY.kr(1, 0), 0.2, MouseX.kr(-1, 1));
}.play;
)
::
]


